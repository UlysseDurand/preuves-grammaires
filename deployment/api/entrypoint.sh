#!/bin/sh
# start Docker daemon in background
dockerd-entrypoint.sh &

# wait for Docker to be ready
until docker info >/dev/null 2>&1; do
  sleep 0.5
done

# finally run FastAPI
exec "$@"