#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

command -v docker >/dev/null || { echo "docker is required" >&2; exit 1; }
command -v k3d >/dev/null || { echo "k3d is required" >&2; exit 1; }
command -v kubectl >/dev/null || { echo "kubectl is required" >&2; exit 1; }

docker build -t tichess:local .

if ! k3d cluster list --no-headers | awk '{print $1}' | grep -qx tichess; then
  k3d cluster create --config k8s/k3d.yaml
fi

k3d image import tichess:local -c tichess
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/stack.yaml

if [[ -n "${LICHESS_TOKEN:-}" ]]; then
  kubectl -n tichess create secret generic lichess-bot \
    --from-literal=token="$LICHESS_TOKEN" \
    --dry-run=client -o yaml | kubectl apply -f -
  kubectl apply -f k8s/bot.yaml
fi

kubectl -n tichess rollout status deployment/model-service --timeout=360s
kubectl -n tichess rollout status deployment/controller-service --timeout=360s
kubectl -n tichess rollout status deployment/view-service --timeout=360s
kubectl -n tichess rollout status deployment/kafka --timeout=360s
kubectl -n tichess rollout status deployment/stream-service --timeout=360s

echo "TiChess is available at http://localhost:8080"
if [[ -z "${LICHESS_TOKEN:-}" ]]; then
  echo "Bot skipped. Export LICHESS_TOKEN and run this script again to deploy it."
fi
