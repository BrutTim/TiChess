# TiChess

TiChess ist ein Schachspiel in Scala mit funktionaler Spiellogik, mehreren Oberflächen und einer Docker-basierten Microservice-Architektur.

## Architektur

Die Anwendung ist in drei Microservices aufgeteilt:

- `model-service`
  - fachliche Spiellogik
  - prüft und berechnet Züge
  - arbeitet zustandslos auf Basis eines übergebenen FEN-Strings
- `controller-service`
  - enthält die Anwendungslogik
  - verwaltet den aktuellen Spielzustand
  - verarbeitet Kommandos wie Züge, Remis, Aufgeben, FEN- und PGN-Import/Export
  - kommuniziert per HTTP mit dem `model-service`
- `view-service`
  - stellt die Weboberfläche bereit
  - liefert die HTML-/JavaScript-Ansicht aus
  - kommuniziert per HTTP mit dem `controller-service`

Datenfluss:

`Browser -> view-service -> controller-service -> model-service`

Damit sind Darstellung, Anwendungssteuerung und fachliche Spiellogik klar getrennt.

## Starten mit Docker Compose

Die gesamte Anwendung wird mit Docker Compose gestartet:

```bash
docker compose up --build
```

Danach ist die Webanwendung unter [http://localhost:8080](http://localhost:8080) erreichbar.

Die drei Services laufen dabei auf:

- `view-service` auf Port `8080`
- `controller-service` auf Port `8082`
- `model-service` auf Port `8081`

Im Hintergrund starten:

```bash
docker compose up --build -d
```

Status prüfen:

```bash
docker compose ps
```

Container stoppen:

```bash
docker compose down
```

## Lokale Entwicklung

Tests:

```bash
sbt test
```

Coverage:

```bash
sbt clean coverage test coverageReport
```

Console-UI starten:

```bash
sbt run
```

ScalaFX-GUI starten:

```bash
sbt "runMain ch.tichess.GuiMain"
```

## Konsolen-Kommandos

- Zug eingeben: `e2 e4`
- Hilfe anzeigen: `help`
- Spiel beenden: `quit`
- Position setzen (FEN, minimal): `fen <placement> <w|b>`
  - Beispiel: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`
