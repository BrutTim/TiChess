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
- 100% Statement- und Branch-Coverage im aktuellen Testsetup

## Architektur

Die Docker-Anwendung ist in drei Services aufgeteilt:

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

Datenfluss:

```text
Browser -> view-service -> controller-service -> model-service
```

Persistenz:

- Docker Compose startet PostgreSQL und MongoDB.
- Standardmäßig verwendet der Controller in Docker `DB_TYPE=mongo`.
- Der Spielstand wird unter der ID `default` gespeichert und beim Neustart wieder geladen.
- Challenges werden beim Start aus den eingebauten Lichess-Fallbacks oder optional aus einer CSV-Datei geladen.

## Starten mit Docker Compose

Die komplette Webanwendung starten:

```bash
docker compose up --build
```

Danach ist die Web-UI unter [http://localhost:8080](http://localhost:8080) erreichbar.

Services und Ports:

- `view-service`: `8080`
- `model-service`: `8081`
- `controller-service`: `8082`
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
Statement coverage: 100.00%
Branch coverage:    100.00%
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
- `MODEL_SERVICE_URL` Standard im Docker-Netzwerk `http://model-service:8081`
- `CONTROLLER_SERVICE_URL` Standard im Docker-Netzwerk `http://controller-service:8082`
- `DB_TYPE` entweder `mongo` oder `postgres`
- `MONGO_URI` Standard `mongodb://mongo-db:27017`
- `DB_URL` Standard `jdbc:postgresql://postgres-db:5432/tichess`
- `DB_USER` Standard `postgres`
- `DB_PASSWORD` Standard `password`
- `LICHESS_PUZZLE_CSV` optionaler Pfad zu einer Lichess-Puzzle-CSV

## HTTP-Endpunkte

Model-Service:

- `POST /api/model/applyMove`

Controller-Service:

- `POST /api/controller/update`
- `GET /api/controller/state`
- `GET /api/controller/challenges`

View-Service:

- `GET /`
- `GET /api/view/game`
- `POST /api/controller/update` als Proxy zur Controller-API

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
