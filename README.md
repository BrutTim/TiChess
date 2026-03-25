# TiChess

Ein einfaches Schachspiel in Scala (MVC, funktional/immutable) mit Console-UI, ScalaFX-GUI, automatisierten Tests und Coverage via scoverage.

## Entwickeln & Ausführen

### Tests

```bash
sbt test
```

### Coverage

```bash
sbt clean coverage test coverageReport
```

### Starten (Console UI)

```bash
sbt run
```

### Starten (ScalaFX GUI)

```bash
sbt "runMain ch.tichess.GuiMain"
```

## Konsolen-Kommandos

- Zug eingeben: `e2 e4`
- Hilfe anzeigen: `help`
- Spiel beenden: `quit`
- Position setzen (FEN, minimal): `fen <placement> <w|b>`
  - Beispiel: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`
