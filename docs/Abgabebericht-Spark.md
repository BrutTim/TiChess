# Abgabebericht: Spark

## 1. Ziel

TiChess verwendet Spark fuer eine dauerhafte White/Black-Bestenliste.
Ausgewertet werden ausschliesslich abgeschlossene Partien:

- Spiele
- Siege
- Remis
- Niederlagen
- Punkte

Ein Sieg ergibt drei Punkte, ein Remis einen Punkt und eine Niederlage null
Punkte.

## 2. Datei als erste Datenquelle

Die Datei `examples/spark-game-events.jsonl` enthaelt strukturierte
Beispielereignisse. Jede Zeile ist ein vollstaendiges JSON-Event.

```bash
sbt "runMain ch.tichess.analytics.ChessSparkAnalytics file examples/spark-game-events.jsonl"
```

Spark liest die Datei mit einem festen Schema. `GameStarted` und `MovePlayed`
dienen als realistische Eingangsdaten, fuer die Bestenliste werden aber nur
`GameFinished`-Events aggregiert.

Beispielausgabe:

```text
player  games  victories  draws  losses  score
Black   3      1          1      1       4
White   3      1          1      1       4
```

## 3. Echte Web-UI-Spiele als Kafka-Events

Die Web-UI sendet ihre Zuege und Kommandos wie bisher an den Controller. Der
Controller erkennt fachlich relevante Zustandswechsel und publiziert
strukturierte Events nach `tichess.game-events`:

```json
{
  "eventId": "2bc...",
  "gameId": "73a...",
  "eventType": "GameFinished",
  "command": "resign",
  "winner": "Black",
  "result": "resignation",
  "moveCount": 12,
  "timestamp": 1718712000000,
  "fen": "..."
}
```

Damit werden nicht nur Testskripte ausgewertet. Normale Zuege, Aufgabe, Remis
und Schachmatt aus der Web-UI gelangen automatisch in Kafka.

## 4. Spark Structured Streaming

Spark liest das Topic fortlaufend:

```scala
spark.readStream
  .format("kafka")
  .option("kafka.bootstrap.servers", bootstrapServers)
  .option("subscribe", "tichess.game-events")
  .option("startingOffsets", "earliest")
  .load()
```

Jede abgeschlossene Partie wird in zwei Auswertungszeilen umgewandelt: eine
fuer White und eine fuer Black. Danach aggregiert Spark die Ergebnisse mit
`groupBy`.

## 5. Speicherung und Web-UI

Jeder vollstaendige Streaming-Batch wird mit Upserts in die MongoDB-Collection
`player_statistics` geschrieben. Ein Dokument sieht beispielsweise so aus:

```json
{
  "_id": "White",
  "games": 8,
  "victories": 4,
  "draws": 2,
  "losses": 2,
  "score": 14,
  "updatedAt": 1718712000000
}
```

Der Controller stellt die Daten unter `GET /api/controller/statistics` bereit.
Der View-Service reicht sie unter `GET /api/view/statistics` weiter. Die
Web-UI aktualisiert ihren Statistik-Tab alle fuenf Sekunden.

## 6. Gesamtarchitektur

```text
Web-UI
  -> Controller
  -> Model
  -> tichess.game-events
  -> Spark Structured Streaming
  -> MongoDB player_statistics
  -> Controller REST
  -> Web-UI Bestenliste
```

## 7. Start und Demo

Die komplette Integration startet automatisch:

```bash
VIEW_HOST_PORT=8084 docker compose up --build -d
```

Danach:

1. Web-UI unter `http://localhost:8084` oeffnen.
2. Eine Partie starten.
3. Einige Zuege spielen und eine Seite aufgeben lassen.
4. Den Statistik-Tab oeffnen.
5. Nach hoechstens fuenf Sekunden erscheint die aktualisierte Bestenliste.

Spark-Logs:

```bash
docker compose logs -f spark-analytics
```

Kafka-Events:

```bash
docker compose exec kafka \
  /opt/kafka/bin/kafka-console-consumer.sh \
  --bootstrap-server localhost:19092 \
  --topic tichess.game-events \
  --from-beginning
```

## 8. Tests

```bash
sbt "testOnly ch.tichess.analytics.ChessSparkAnalyticsSpec"
```

Der Test prueft:

- White/Black-Aggregation aus abgeschlossenen Partien
- Sieg, Remis, Niederlage und Punktestand
- Erzeugung eines strukturierten Spielende-Events
- Ignorieren nicht spielrelevanter Kommandos
