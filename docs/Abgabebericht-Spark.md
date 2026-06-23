# Abgabebericht: Spark

## 1. Ziel

TiChess wurde um eine Spark-Auswertung fuer die bereits vorhandenen
Stream-/Kafka-Ergebnisse erweitert. Spark liest Ereignisse im Format von
`StreamCommandResult` und berechnet daraus einfache Spielmetriken:

- Anzahl erfolgreicher und fehlgeschlagener Events
- Anzahl Siege pro Gewinnerfarbe
- Remis-Zahl
- Score pro Gewinnerfarbe

Der Score ist bewusst einfach gehalten:

- Sieg: 3 Punkte
- Remis: 1 Punkt
- normales erfolgreiches Kommando: 0 Punkte
- fehlgeschlagenes Kommando: -1 Punkt

Da TiChess aktuell keine benannten Spieler in den Stream-Events fuehrt,
verwendet die Auswertung `White` und `Black` als Spieler-Metrik.

## 2. Datei als erste Datenquelle

Die Beispielereignisse liegen als JSON Lines in:

```text
examples/spark-game-events.jsonl
```

Start:

```bash
sbt "runMain ch.tichess.analytics.ChessSparkAnalytics file examples/spark-game-events.jsonl"
```

Spark liest die Datei mit einem festen Schema, erweitert die Events um
`winner`, `draw` und `score` und aggregiert danach mit `groupBy`.

## 3. Kafka als Spark-Stream

Der bestehende `stream-service` schreibt verarbeitete Kommandos in das Topic:

```text
tichess.events
```

Spark kann dieses Topic direkt als Structured Stream lesen:

```bash
docker compose up --build -d
sbt "runMain ch.tichess.analytics.ChessSparkAnalytics kafka localhost:9092 tichess.events"
```

Danach koennen Kommandos wie bisher an Kafka gesendet werden:

```bash
curl -H "Content-Type: text/plain" \
  --data-binary @examples/chess-commands.dsl \
  http://localhost:8083/api/kafka/commands
```

Die Spark-Ausgabe aktualisiert die Aggregation im Console Sink alle fuenf
Sekunden.

## 4. Implementierung

Wesentliche Dateien:

- `src/main/scala/ch/tichess/analytics/ChessSparkAnalytics.scala`
- `src/test/scala/ch/tichess/analytics/ChessSparkAnalyticsSpec.scala`
- `examples/spark-game-events.jsonl`

Die Spark-Kafka-Anbindung verwendet:

```scala
spark.readStream
  .format("kafka")
  .option("kafka.bootstrap.servers", bootstrapServers)
  .option("subscribe", topic)
  .option("startingOffsets", "earliest")
  .load()
```

Anschliessend wird `value` nach `String` gecastet und mit `from_json` in das
gleiche Schema geparst, das auch fuer die Datei verwendet wird.

## 5. Test

Die Aggregation ist mit einem Spark-Unit-Test abgedeckt:

```bash
sbt "testOnly ch.tichess.analytics.ChessSparkAnalyticsSpec"
```

Der Test prueft, dass Siege fuer White und Black erkannt und korrekt bewertet
werden und dass fehlgeschlagene Events in der Gruppe `No winner` landen.
