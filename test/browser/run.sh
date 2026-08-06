#!/bin/sh
# End-to-end browser test: builds the app, serves it, and drives a real
# headless Chrome through both thread modes.
set -e
cd "$(dirname "$0")/../.."

CHROME=${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}
[ -x "$CHROME" ] || { echo "browser test skipped: no Chrome at $CHROME"; exit 0; }

dune build @default

WORK=$(mktemp -d)
PROFILE=$(mktemp -d)
PORT=${PORT:-8973}
trap 'kill $CHROME_PID 2>/dev/null; rm -rf "$WORK" "$PROFILE" 2>/dev/null; true' EXIT

cp _build/default/web/app.bc.js _build/default/web/three-view.js "$WORK/"
cp test/browser/driver.js "$WORK/"
sed 's#</body>#<script src="driver.js"></script></body>#' _build/default/web/index.html \
  > "$WORK/index.html"

# Give the server a moment to bind before Chrome asks it for the page.
(
  sleep 2
  # swiftshader so the 3D tab has a WebGL context to render into
  "$CHROME" --headless=new --use-gl=swiftshader --enable-unsafe-swiftshader \
    --no-first-run --no-default-browser-check \
    --window-size=${WINDOW:-1440,900} \
    --user-data-dir="$PROFILE" "http://127.0.0.1:$PORT/index.html" >/dev/null 2>&1
) &
CHROME_PID=$!

python3 test/browser/serve.py "$PORT" "$WORK"
