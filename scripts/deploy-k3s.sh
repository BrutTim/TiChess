#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

command -v docker >/dev/null || { echo "docker is required" >&2; exit 1; }
command -v k3s >/dev/null || { echo "k3s is required" >&2; exit 1; }

docker build -t tichess:local .
docker save tichess:local | sudo k3s ctr images import -

sudo k3s kubectl apply -f k8s/namespace.yaml
sudo k3s kubectl apply -f k8s/stack.yaml

if [[ -n "${LICHESS_TOKEN:-}" ]]; then
  sudo k3s kubectl -n tichess create secret generic lichess-bot \
    --from-literal=token="$LICHESS_TOKEN" \
    --dry-run=client -o yaml | sudo k3s kubectl apply -f -
  sudo k3s kubectl apply -f k8s/bot.yaml
fi

sudo k3s kubectl -n tichess rollout status deployment/model-service --timeout=360s
sudo k3s kubectl -n tichess rollout status deployment/controller-service --timeout=360s
sudo k3s kubectl -n tichess rollout status deployment/view-service --timeout=360s
sudo k3s kubectl -n tichess rollout status deployment/kafka --timeout=360s
sudo k3s kubectl -n tichess rollout status deployment/stream-service --timeout=360s

echo "TiChess is available on this server's port 80."
if [[ -z "${LICHESS_TOKEN:-}" ]]; then
  echo "Bot skipped. Export LICHESS_TOKEN and run this script again to deploy it."
fi
