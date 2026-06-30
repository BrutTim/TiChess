# TiChess

TiChess ist ein Schachprojekt in Scala 3.3.4. Es kombiniert eine funktionale Schach-Engine, Konsolen- und ScalaFX-Oberflächen sowie eine Docker-basierte Web-UI mit getrennten Model-, Controller- und View-Services.

## Features

- vollständige Schachlogik mit legalen Zügen, Schach, Schachmatt, Patt, Rochade, en passant, Promotion und 50-Züge-Regel
- FEN-Import und -Export
- PGN-Import und -Export mit mehreren Parser-Implementierungen
- Remis anbieten, annehmen und ablehnen
- Aufgabe und neues Spiel
- Zughistorie, geschlagene Figuren und Materialanzeige
- Puzzle-/Challenge-Training mit Lichess-Puzzle-Seeds
- persistenter Spielstand über Datenbank
- Web-UI, Console-UI und lokale ScalaFX-GUI
- 99,18% Statement- und 99,26% Branch-Coverage im aktuellen Testsetup

## Architektur

Die Docker-Anwendung ist in fünf Anwendungsservices aufgeteilt:

- `model-service`
  - führt die fachliche Spiellogik aus
  - prüft und berechnet Züge
  - arbeitet zustandslos auf Basis eines übergebenen FEN-Strings
- `controller-service`
  - verwaltet den aktuellen Spielzustand
  - verarbeitet Kommandos wie Züge, Remis, Aufgabe, neues Spiel, Challenges sowie FEN/PGN
  - speichert und lädt den Spielstand über eine Datenbank
  - kommuniziert per HTTP mit dem `model-service`
- `view-service`
  - stellt die Weboberfläche bereit
  - liefert HTML, CSS und JavaScript aus
  - kommuniziert per HTTP mit dem `controller-service`
- `stream-service`
  - verarbeitet mehrzeilige Schachkommandos als Akka Reactive Stream
  - stellt einen Kafka Producer und Consumer bereit
  - konsumiert `tichess.commands` und publiziert Ergebnisse nach `tichess.events`
  - kommuniziert per HTTP mit dem `controller-service`
- `spark-analytics`
  - konsumiert strukturierte Spielereignisse aus `tichess.game-events`
  - berechnet eine Bestenliste für White und Black
  - speichert Spiele, Siege, Remis, Niederlagen und Punkte in MongoDB
  - stellt die Daten indirekt über Controller und Web-UI bereit

Datenfluss:

```text
Browser -> view-service -> controller-service -> model-service
                                |
                                v
                         tichess.game-events
                                |
                                v
                       spark-analytics -> MongoDB -> Web-UI

DSL/Kafka -> stream-service -> controller-service -> model-service
```

Persistenz:

- Docker Compose und Kubernetes starten MongoDB.
- Der Controller verwendet in den Deployment-Setups `DB_TYPE=mongo`.
- Der Spielstand wird unter der ID `default` gespeichert und beim Neustart wieder geladen.
- Challenges werden beim Start aus den eingebauten Lichess-Fallbacks oder optional aus einer CSV-Datei geladen.

## Starten mit Docker Compose

Die komplette Webanwendung starten:

```bash
docker compose up --build
```

Danach ist die Web-UI unter [http://localhost:8080](http://localhost:8080) erreichbar.

Falls parallel der lokale k3d-Cluster läuft, ist Port `8080` bereits durch
den k3d-Ingress belegt. Compose kann dann auf einem anderen Host-Port
gestartet werden:

```bash
VIEW_HOST_PORT=8084 docker compose up --build -d
```

Die Compose-Web-UI ist in diesem Fall unter
[http://localhost:8084](http://localhost:8084) erreichbar. Der interne
Container-Port bleibt `8080`.

Services und Ports:

- `view-service`: `8080`
- `model-service`: `8081`
- `controller-service`: `8082`
- `stream-service`: `8083`
- `kafka`: `9092`
- `postgres-db`: `5432`
- `mongo-db`: `27017`

Im Hintergrund starten:

```bash
docker compose up --build -d
```

Status prüfen:

```bash
docker compose ps
```

Stoppen:

```bash
docker compose down
```

Stoppen inklusive Datenbank-Volumes:

```bash
docker compose down -v
```

## Reactive Streams und Kafka

Die ausführliche Abgabedokumentation liegt unter
[`docs/Abgabebericht-Reactive-Streams-Kafka.md`](docs/Abgabebericht-Reactive-Streams-Kafka.md).

Direkte Source-Flow-Sink-Pipeline testen:

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8083/api/stream/commands
```

Dieselben Kommandos an den Kafka Producer senden:

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8083/api/kafka/commands
```

Komplette k3d-Demo:

```bash
./scripts/demo-stream-kafka.sh
```

## Spark Analytics

Die Spark-Abgabedokumentation liegt unter
[`docs/Abgabebericht-Spark.md`](docs/Abgabebericht-Spark.md).

Datei-basierte Auswertung starten:

```bash
sbt "runMain ch.tichess.analytics.ChessSparkAnalytics file examples/spark-game-events.jsonl"
```

Kafka-Stream aus dem fachlichen Topic `tichess.game-events` lesen:

```bash
sbt "runMain ch.tichess.analytics.ChessSparkAnalytics kafka localhost:9092 tichess.game-events"
```

Bei `docker compose up --build` startet `spark-analytics` automatisch. Echte
Züge und Spielenden aus der Web-UI werden vom Controller nach Kafka
veröffentlicht. Die daraus berechnete White/Black-Bestenliste erscheint im
Statistik-Tab der Web-UI.

## Lichess-Bot mit Docker Compose

Der Bot wird über ein Compose-Profil gestartet, damit für den normalen Web-Stack
kein Lichess-Token erforderlich ist:

```bash
export LICHESS_TOKEN=lip_your_token_here
docker compose --profile bot up --build -d
docker compose logs -f lichess-bot
```

Alternativ kann `.env.example` als Vorlage für eine lokale `.env` verwendet
werden. `.env` ist aus Git ausgeschlossen. Der Bot läuft ohne interaktive
Standardeingabe dauerhaft weiter und wird mit `docker compose down` sauber
beendet.

Optional können Syzygy-Tablebases eingebunden werden:

```bash
export SYZYGY_PATH=/absolute/path/to/tablebases
docker compose --profile bot up --build -d
```

## NowChess-Turnierserver-Bot

Zusätzlich zum Lichess-Bot kann TiChess gegen den NowChess-Turnierserver
laufen. Die API wird aus dem öffentlichen `maichess/tournament-server`-Repo
verwendet:

- Registrierung: `POST /api/auth/register`
- Turnierbeitritt: `POST /api/tournament/{id}/join`
- Turnierstream: `GET /api/tournament/{id}/stream`
- Spielstream: `GET /api/tournament/{id}/game/{gameId}/stream`
- Zug senden: `POST /api/tournament/{id}/game/{gameId}/move/{uci}`

Direkt lokal:

```bash
TOURNAMENT_ID=tournament_id_here sbt "run tournament"
```

Mit bereits vorhandenem Turnierserver-Token:

```bash
TOURNAMENT_SERVER_URL=https://tournament.staging.maichess.berger-software.com \
TOURNAMENT_ID=tournament_id_here \
TOURNAMENT_TOKEN=jwt_from_tournament_server \
sbt "run tournament"
```

Mit Docker Compose:

```bash
TOURNAMENT_ID=tournament_id_here \
TOURNAMENT_TOKEN=jwt_from_tournament_server \
docker compose --profile tournament up --build -d
```

Mit k3d oder k3s:

```bash
export TOURNAMENT_ID=tournament_id_here
export TOURNAMENT_TOKEN=jwt_from_tournament_server
export TOURNAMENT_SERVER_URL=https://tournament.staging.maichess.berger-software.com
./scripts/deploy-k3d.sh
kubectl -n tichess logs -f deployment/tournament-bot
```

Falls `TOURNAMENT_TOKEN` fehlt, registriert sich der Bot einmalig mit
`TOURNAMENT_BOT_NAME` und gibt den erzeugten Token im Log aus. Diesen Token
sollte man anschließend als `TOURNAMENT_TOKEN` wiederverwenden.

## Lokales Kubernetes mit k3d

Voraussetzungen:

- laufender Docker-Daemon
- `kubectl`
- `k3d`, unter macOS zum Beispiel mit `brew install k3d`

Web-Stack bauen, Cluster erzeugen, Image importieren und deployen:

```bash
./scripts/deploy-k3d.sh
```

Danach ist TiChess unter [http://localhost:8080](http://localhost:8080)
erreichbar.

Den Bot im selben Cluster deployen:

```bash
export LICHESS_TOKEN=lip_your_token_here
./scripts/deploy-k3d.sh
kubectl -n tichess logs -f deployment/lichess-bot
```

Status und Aufräumen:

```bash
kubectl -n tichess get all
k3d cluster delete tichess
```

Die Kubernetes-Dateien liegen unter `k8s/`:

- `namespace.yaml`: eigener Namespace
- `stack.yaml`: MongoDB, Model, Controller, View, Services und Ingress
- `bot.yaml`: Lichess-Bot mit Token aus einem Kubernetes Secret
- `k3d.yaml`: lokaler Cluster mit Portweiterleitung `8080 -> 80`

## Deployment auf dem virtuellen Server

Zielserver dieser Abgabe:

```text
141.37.74.150
```

Auf dem Server müssen Docker und k3s installiert sein. Nach dem Checkout des
Repositories wird der Stack direkt auf dem Server gebaut und in die
k3s-containerd-Registry importiert:

```bash
ssh <benutzer>@141.37.74.150
cd TiChess
export LICHESS_TOKEN=lip_your_token_here
./scripts/deploy-k3s.sh
```

Anschließend:

```bash
sudo k3s kubectl -n tichess get pods
sudo k3s kubectl -n tichess logs -f deployment/lichess-bot
curl http://141.37.74.150/health
```

Die Weboberfläche wird über den von k3s mitgelieferten Traefik-Ingress auf
[http://141.37.74.150](http://141.37.74.150) veröffentlicht. Der echte
Lichess-Token wird ausschließlich als Kubernetes Secret angelegt und steht
nicht in den Manifesten.

## Lokale Entwicklung

Voraussetzungen:

- JDK 21 oder neuer
- sbt
- Docker, falls die Microservices lokal per Compose gestartet werden sollen

Tests:

```bash
sbt test
```

Coverage:

```bash
sbt clean coverage test coverageReport
```

Der HTML-Report liegt danach unter:

```text
target/scala-3.3.4/scoverage-report/index.html
```

Aktueller Stand:

```text
Statement coverage: 99.18%
Branch coverage:    99.26%
```

## Oberflächen

Console-UI starten:

```bash
sbt run
```

Console-UI mit Skript-Kommandos starten:

```bash
sbt "run e2 e4 e7 e5 quit"
```

ScalaFX-GUI starten:

```bash
sbt "runMain ch.tichess.GuiMain"
```

Web-UI starten:

```bash
docker compose up --build
```

Die Web-UI bietet Spiel, Import/Export und Challenge-Training in getrennten Tabs. `Partie starten` erzeugt ein neues Spiel und startet die lokale Uhr.

## Konsolen-Kommandos

Züge:

```text
e2 e4
e7 e8 q
```

Allgemein:

```text
help
quit
new
resign
draw
accept
decline
```

FEN:

```text
fen import <fen>
fen <fen>
fen export
```

Beispiel:

```text
fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w
```

PGN:

```text
pgn import <pgn>
pgn export
```

Parser:

```text
parser
parser fastparse
parser combinators
parser regex
```

Challenges:

```text
challenge random
challenge load <id>
```

## Datenbanken und Konfiguration

Wichtige Umgebungsvariablen:

- `MODEL_SERVICE_PORT` Standard `8081`
- `CONTROLLER_SERVICE_PORT` Standard `8082`
- `VIEW_SERVICE_PORT` Standard `8080`
- `STREAM_SERVICE_PORT` Standard `8083`
- `MODEL_SERVICE_URL` Standard im Docker-Netzwerk `http://model-service:8081`
- `CONTROLLER_SERVICE_URL` Standard im Docker-Netzwerk `http://controller-service:8082`
- `DB_TYPE` entweder `mongo` oder `postgres`
- `MONGO_URI` Standard `mongodb://mongo-db:27017`
- `DB_URL` Standard `jdbc:postgresql://postgres-db:5432/tichess`
- `DB_USER` Standard `postgres`
- `DB_PASSWORD` Standard `password`
- `LICHESS_PUZZLE_CSV` optionaler Pfad zu einer Lichess-Puzzle-CSV
- `LICHESS_TOKEN` Token des Lichess-Bot-Kontos
- `SYZYGY_PATH` optionaler Pfad zu Syzygy-Tablebases
- `KAFKA_BOOTSTRAP_SERVERS` Standard lokal `localhost:9092`
- `KAFKA_COMMANDS_TOPIC` Standard `tichess.commands`
- `KAFKA_EVENTS_TOPIC` Standard `tichess.events`
- `KAFKA_GAME_EVENTS_TOPIC` Standard `tichess.game-events`
- `KAFKA_CONSUMER_GROUP` Standard `tichess-stream-service`

## HTTP-Endpunkte

Model-Service:

- `POST /api/model/applyMove`

Controller-Service:

- `POST /api/controller/update`
- `GET /api/controller/state`
- `GET /api/controller/challenges`

View-Service:

- `GET /`
- `GET /health`
- `GET /api/view/game`
- `POST /api/controller/update` als Proxy zur Controller-API

Model- und Controller-Service stellen ebenfalls `GET /health` für
Container- und Kubernetes-Probes bereit.

Stream-Service:

- `GET /health`
- `POST /api/stream/commands`
- `POST /api/kafka/commands`

## Projektstruktur

```text
src/main/scala/ch/tichess/model        Schachmodell, Regeln, FEN, PGN
src/main/scala/ch/tichess/controller   Kommandos, AppState, Spielsteuerung
src/main/scala/ch/tichess/services     HTTP-Services und Service-Clients
src/main/scala/ch/tichess/view         Console-, Web- und JSON-View-Code
src/main/scala/ch/tichess/controller/persistence
                                      Game- und Challenge-Persistenz
src/test/scala/ch/tichess              Tests und Coverage-Spezifikationen
```

## Hinweise

- Im Docker-Setup wird JavaFX deaktiviert (`INCLUDE_JAVAFX=false`), weil dort nur die Webservices gebaut und gestartet werden.
- `MongoGameDao`, `MongoChallengeDao`, Web-/GUI-Klassen, Services und FastParse-Parser sind im Coverage-Setup ausgeschlossen, weil sie entweder externe Infrastruktur, UI-Laufzeit oder generierte/adapterartige Integrationspfade betreffen.
- Für lokale Datenbanktests wird H2 über Slick verwendet.
