#!/usr/bin/env bash
set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8080}"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DSL_FILE="${1:-$ROOT_DIR/examples/chess-commands.dsl}"

echo "Reactive Streams result:"
curl -fsS \
  -H "Content-Type: text/plain" \
  --data-binary "@$DSL_FILE" \
  "$BASE_URL/api/stream/commands"
echo

echo "Kafka producer result:"
curl -fsS \
  -H "Content-Type: text/plain" \
  --data-binary "@$DSL_FILE" \
  "$BASE_URL/api/kafka/commands"
echo

echo "Kafka events:"
sleep 3
kubectl -n tichess exec deployment/kafka -- \
  /opt/kafka/bin/kafka-console-consumer.sh \
  --bootstrap-server kafka:9092 \
  --topic tichess.events \
  --from-beginning \
  --timeout-ms 5000 || true
