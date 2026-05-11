#!/usr/bin/env bash
set -euo pipefail

# Ensure storage exists before we try to inspect or patch cluster metadata.
/opt/cronicle/bin/control.sh setup >/dev/null 2>&1 || true

# Cronicle stores manager candidates by hostname+IP. In Dokku the container IP
# changes across deploys, so refresh the persisted IP for the fixed legacy
# hostname before manager election runs.
node /opt/cronicle/refresh-server-ip.js

exec /opt/cronicle/bin/manager "$@"
