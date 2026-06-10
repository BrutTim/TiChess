# Abgabebericht: Reactive Streams und Kafka

## 1. Ziel der Erweiterung

TiChess wurde um zwei zusammenhängende Architekturbausteine erweitert:

1. Eine Reactive-Streams-Pipeline verarbeitet eine mehrzeilige externe
   Schach-DSL mit Akka Streams.
2. Kafka dient als Event Bus zwischen einem Producer, der
   Schachkommandos veröffentlicht, und einem Consumer, der diese Kommandos
   über den bestehenden Controller-Microservice verarbeitet.

Die fachliche Schachlogik bleibt ausschließlich im bestehenden
`controller-service` und `model-service`. Der neue `stream-service`
orchestriert die asynchrone Verarbeitung und dupliziert keine Domänenlogik.

## 2. Gesamtarchitektur

```text
Direkter Reactive-Streams-Pfad

HTTP DSL
   |
   v
Source[String]
   |
   v
cleanupFlow
   |
   v
validationFlow
   |
   v
processingFlow.mapAsync(1)
   |
   +------ HTTP ------> controller-service ------> model-service
   |
   v
Sink.seq[StreamCommandResult]
   |
   v
HTTP JSON response
```

```text
Kafka-Pfad

HTTP DSL
   |
   v
Akka Source + validation Flows
   |
   v
Kafka Producer
   |
   v
tichess.commands
   |
   v
Alpakka Kafka Consumer Source
   |
   v
processingFlow / controller HTTP call
   |
   v
Kafka Producer Sink + offset commit
   |
   v
tichess.events
```

## 3. Reactive-Streams-Aufgabe

### 3.1 Source

Die Eingabe ist eine externe textuelle DSL. Jede Zeile enthält ein bereits
von TiChess unterstütztes Kommando, zum Beispiel:

```text
# comment
new
e2 e4
e7 e5
```

Der HTTP-Request-Body oder die Beispieldatei
`examples/chess-commands.dsl` wird in eine
`Source[String, NotUsed]` überführt. Durch `zipWithIndex` erhält jede Zeile
eine nachvollziehbare Zeilennummer.

### 3.2 Flows

Die Pipeline besitzt mehrere explizite Flows:

- `cleanupFlow`
  - entfernt Leerzeichen
  - ignoriert leere Zeilen
  - ignoriert Kommentare mit `#` oder `//`
  - bewahrt die ursprüngliche Zeilennummer
- `validationFlow`
  - verwendet den bestehenden `Command.parse`
  - trennt gültige und ungültige DSL-Kommandos
  - behandelt Parserfehler als Daten und beendet nicht den ganzen Stream
- `processingFlow`
  - verarbeitet gültige Kommandos asynchron
  - ruft den `controller-service` per HTTP auf
  - verwendet `mapAsync(1)`, damit die Reihenfolge der Spielzüge garantiert
    bleibt
  - wandelt auch Controller-Ausfälle in strukturierte Fehlerergebnisse um

`mapAsync(1)` ist für eine Schachpartie entscheidend. `e7 e5` darf nicht
parallel oder vor `e2 e4` verarbeitet werden.

### 3.3 Sink

Der direkte HTTP-Pfad materialisiert den Graphen mit:

```scala
runWith(Sink.seq)
```

Der Sink sammelt alle Ergebnisse in Reihenfolge. Die HTTP-Antwort enthält
für jede relevante Eingabezeile:

- Zeilennummer
- ursprüngliches Kommando
- Ergebnis der syntaktischen Validierung
- Erfolg der fachlichen Verarbeitung
- optionale Meldung
- resultierende FEN

### 3.4 Back Pressure

Akka Streams verbindet Source, Flows und Sink über Reactive Streams.
Nachgelagerte Stufen fordern nur so viele Elemente an, wie sie verarbeiten
können. Zusätzlich begrenzt `mapAsync(1)` die Zahl gleichzeitig laufender
Controller-Aufrufe auf exakt einen.

Damit kann ein großes DSL-Skript den Controller nicht mit unbeschränkt vielen
parallelen HTTP-Requests überlasten.

## 4. Kafka-Aufgabe

### 4.1 Topics

Es werden zwei Topics verwendet:

- `tichess.commands`
  - enthält validierte Schachkommandos
  - Key: aktuell `default`, vorgesehen als Spiel-ID
  - Value: TiChess-Kommando, zum Beispiel `e2 e4`
- `tichess.events`
  - enthält das strukturierte Verarbeitungsergebnis als JSON
  - derselbe Key hält die Ereignisse einer Partie in derselben Partition

Beide Topics besitzen im Abgabe-Setup eine Partition. Dadurch ist die
Reihenfolge der Kommandos garantiert.

### 4.2 Scala Producer

`KafkaCommandProducer` verwendet:

```scala
Source(...)
  .map(command => new ProducerRecord(...))
  .runWith(Producer.plainSink(settings))
```

Vor dem Publish durchlaufen die Zeilen dieselben Cleanup- und
Validierungs-Flows wie der direkte Stream. Ungültige Zeilen werden nicht an
Kafka gesendet, sondern direkt in der Producer-Antwort als `rejected`
ausgegeben.

### 4.3 Scala Consumer

`KafkaCommandBridge` verwendet eine
`Consumer.committableSource`. Jedes Kafka-Kommando wird:

1. aus `tichess.commands` gelesen,
2. durch die Reactive-Streams-Verarbeitung geschickt,
3. per HTTP an den Controller-Microservice übergeben,
4. als JSON in `tichess.events` geschrieben,
5. erst danach über einen committable Producer Sink bestätigt.

Der Offset wird somit erst nach erfolgreichem Schreiben des Ergebnis-Events
committed. Das entspricht einer At-least-once-Verarbeitung.

### 4.4 Kopplung an die Microservices

Kafka greift nicht direkt auf Schachklassen oder Datenbanken zu. Der Consumer
verwendet `ControllerHttpClient`:

```text
Kafka Consumer -> stream-service -> controller-service -> model-service
```

Damit bleiben die vorhandenen Servicegrenzen erhalten:

- Model: Regeln und Zuganwendung
- Controller: Spielzustand und Persistenz
- View: Weboberfläche
- Stream: asynchrone DSL- und Event-Verarbeitung

## 5. HTTP-Schnittstellen

### Direkte Reactive-Streams-Verarbeitung

```http
POST /api/stream/commands
Content-Type: text/plain
```

Beispiel:

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8083/api/stream/commands
```

Im k3d-/Server-Ingress lautet die URL:

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8080/api/stream/commands
```

### Kafka Producer

```http
POST /api/kafka/commands
Content-Type: text/plain
```

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8080/api/kafka/commands
```

### Kafka Events ansehen

k3d:

```bash
kubectl -n tichess exec deployment/kafka -- \
  /opt/kafka/bin/kafka-console-consumer.sh \
  --bootstrap-server kafka:9092 \
  --topic tichess.events \
  --from-beginning \
  --timeout-ms 10000
```

Docker Compose:

```bash
docker compose exec kafka \
  /opt/kafka/bin/kafka-console-consumer.sh \
  --bootstrap-server localhost:19092 \
  --topic tichess.events \
  --from-beginning \
  --timeout-ms 10000
```

## 6. Deployment

### Docker Compose

`docker-compose.yml` enthält zusätzlich:

- `kafka`: Apache Kafka 3.9.1 im KRaft-Modus ohne ZooKeeper
- `kafka-init`: legt beide Topics idempotent an
- `stream-service`: startet Reactive Streams Producer und Consumer
- `kafka-data`: persistentes Kafka-Volume

Start:

```bash
docker compose up --build -d
docker compose ps
```

### Kubernetes/k3d

`k8s/stack.yaml` enthält zusätzlich:

- Kafka Deployment und Service
- Topic-Init-Container im Stream-Service
- Stream-Service Deployment und Service
- Ingress-Routing für `/api/stream` und `/api/kafka`
- Readiness-, Startup- und Liveness-Probes
- Ressourcenanforderungen und Limits

Start:

```bash
./scripts/deploy-k3d.sh
kubectl -n tichess get pods
```

Deployment auf dem virtuellen Server:

```bash
ssh <benutzer>@141.37.74.150
cd TiChess
./scripts/deploy-k3s.sh
```

## 7. Tests und Nachweise

### Automatisierte Tests

`ChessCommandStreamSpec` prüft:

- Entfernen von Leerzeilen und Kommentaren
- Erhalt korrekter Zeilennummern
- Validierung gültiger und ungültiger Kommandos
- sequenzielle Übergabe an den Controller
- Sammeln der Ergebnisse im Sink
- Umwandlung eines Controller-Ausfalls in ein Ergebnis, ohne Stream-Abbruch

Der vollständige Testlauf umfasst 153 erfolgreiche Tests. Der aktuelle
Coverage-Report weist 99,18 % Statement-Coverage und 99,26 % Branch-Coverage
für das Gesamtprojekt aus. Die neu hinzugefügten Klassen
`ChessCommandStream` und `JsonSupport` erreichen jeweils 100 % Statement- und
Branch-Coverage. Die verbleibenden nicht abgedeckten Zweige liegen in bereits
vorhandenen Bot- und Bitboard-Hilfsmethoden.

Ausgeführt mit:

```bash
sbt "testOnly ch.tichess.streaming.ChessCommandStreamSpec"
```

### Durchgeführter k3d-End-to-End-Test

Der reale Cluster lief mit:

```text
controller-service   1/1 Running
kafka                1/1 Running
model-service        1/1 Running
mongo                1/1 Running
stream-service       1/1 Running
view-service         1/1 Running
```

Direkter Stream-Test:

- `new`, `e2 e4` und `e7 e5` wurden erfolgreich verarbeitet.
- `invalid command` wurde mit Parserfehler zurückgegeben.
- Der Stream lief nach dem ungültigen Element weiter.

Kafka-Test:

- Producer-Antwort: drei Kommandos veröffentlicht, ein Kommando abgelehnt.
- Consumer verarbeitete die drei Kommandos in Reihenfolge.
- Im Topic `tichess.events` wurden drei JSON-Ergebnisse mit den
  entsprechenden FEN-Zuständen gelesen.

## 8. Geänderte Dateien

Wesentliche neue Dateien:

- `src/main/scala/ch/tichess/streaming/ChessCommandStream.scala`
- `src/main/scala/ch/tichess/streaming/KafkaCommandProducer.scala`
- `src/main/scala/ch/tichess/streaming/KafkaCommandBridge.scala`
- `src/main/scala/ch/tichess/streaming/StreamServer.scala`
- `src/test/scala/ch/tichess/streaming/ChessCommandStreamSpec.scala`
- `examples/chess-commands.dsl`
- `scripts/demo-stream-kafka.sh`

Wesentliche erweiterte Dateien:

- `build.sbt`
- `docker-compose.yml`
- `k8s/stack.yaml`
- `scripts/deploy-k3d.sh`
- `scripts/deploy-k3s.sh`
- `src/main/scala/ch/tichess/view/JsonSupport.scala`
- `README.md`

## 9. Erfüllung der Aufgabenstellung

Reactive Streams:

- Source vorhanden
- mehrere Flows vorhanden
- Sink vorhanden
- externe DSL vorhanden
- Back Pressure durch Akka Streams
- Verarbeitungsergebnisse werden gesammelt

Kafka:

- Scala Producer vorhanden
- Scala Consumer vorhanden
- Kafka-Topics vorhanden
- Kafka ist über Akka Streams angebunden
- Consumer ist an die bestehenden Microservices gekoppelt
- Docker- und Kubernetes-Deployment vorhanden
- automatisierte und reale Integrationstests vorhanden
