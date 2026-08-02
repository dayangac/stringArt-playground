#!/usr/bin/env python3
"""Serve the built web app and wait for the driver to POST its report.

Exits 0 when the report says every check passed, 1 otherwise -- so run.sh can
just check the exit status.
"""
import base64
import json
import os
import sys
import threading
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

PORT = int(sys.argv[1])
ROOT = sys.argv[2]
report = {}
done = threading.Event()


class Handler(SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        super().__init__(*a, directory=ROOT, **kw)

    def do_POST(self):
        body = self.rfile.read(int(self.headers["Content-Length"]))
        report.update(json.loads(body))
        self.send_response(204)
        self.end_headers()
        done.set()

    def log_message(self, *a):
        pass


def check(report):
    """The report has to describe two real windings, not just a page that loaded."""
    problems = []
    if not report.get("ok"):
        problems.append("driver failed: %s" % report.get("error"))
    lay = report.get("layout") or {}
    if not lay:
        problems.append("no layout report")
    else:
        if lay["scrollHeight"] > lay["innerHeight"] + 1:
            problems.append("page scrolls vertically: %s content in a %s viewport"
                            % (lay["scrollHeight"], lay["innerHeight"]))
        if lay["scrollWidth"] > lay["innerWidth"] + 1:
            problems.append("page scrolls horizontally: %s content in a %s viewport"
                            % (lay["scrollWidth"], lay["innerWidth"]))
        if lay["resultVisible"] > lay["innerHeight"] + 1:
            problems.append("the wound canvas is cut off at the bottom (%s > %s)"
                            % (lay["resultVisible"], lay["innerHeight"]))
    b = report.get("baseline") or {}
    if b.get("algorithm") != "greedy" or float(b.get("economy", 1)) != 0 \
            or float(b.get("lambda", 1)) != 0:
        problems.append("baseline preset did not reset the levers: %s" % b)
    if not report.get("noBoardControl"):
        problems.append("the board colour control is still on the page")
    runs = {r["mode"]: r for r in report.get("runs", [])}
    for mode in ("grayscale", "colour"):
        r = runs.get(mode)
        if r is None:
            problems.append("%s: no run" % mode)
            continue
        if r["status"] != "Done.":
            problems.append("%s: status %r" % (mode, r["status"]))
        if not (r["chords"] and r["chords"] > 100):
            problems.append("%s: only %s chords" % (mode, r["chords"]))
        # stat-match is SSIM against the target now, not a percentage
        if not (r["match"] and r["match"] > 0.05):
            problems.append("%s: ssim only %s" % (mode, r["match"]))
        if r["dark"] < 0.05:
            problems.append("%s: canvas looks blank (%.3f dark)" % (mode, r["dark"]))
        if r["svgLines"] != r["chords"]:
            problems.append("%s: svg has %s lines for %s chords" % (mode, r["svgLines"], r["chords"]))
        if r["seq"] < 100:
            problems.append("%s: winding instructions look empty" % mode)
        if not r["thread"].endswith(" m"):
            problems.append("%s: thread length %r" % (mode, r["thread"]))
        # the thing the project is trying to minimise has to be on screen
        if not r.get("threadNote", "").endswith("per chord"):
            problems.append("%s: thread headline has no per-chord figure (%r)"
                            % (mode, r.get("threadNote")))
    return problems


server = ThreadingHTTPServer(("127.0.0.1", PORT), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
if not done.wait(timeout=180):
    print("browser test: timed out waiting for the driver")
    sys.exit(1)
server.shutdown()

out = os.environ.get("SELFTEST_PNG")
if out:
    for r in report.get("runs", []):
        png = r.get("png", "").split(",", 1)
        if len(png) == 2:
            path = "%s-%s.png" % (out, r["mode"])
            open(path, "wb").write(base64.b64decode(png[1]))
            print("  wrote " + path)

problems = check(report)
for r in report.get("runs", []):
    print(
        "  %-9s %s chords  %s  ssim %s  %s svg lines  %sms"
        % (r["mode"], r["chords"], r["thread"], r["match"], r["svgLines"], r.get("ms"))
    )
if problems:
    print("browser test FAILED")
    for p in problems:
        print("  - " + p)
    trimmed = {k: v for k, v in report.items() if k != "runs"}
    trimmed["runs"] = [{k: v for k, v in r.items() if k != "png"} for r in report.get("runs", [])]
    print(json.dumps(trimmed, indent=2)[:2000])
    sys.exit(1)
lay = report.get("layout") or {}
print("  layout    %sx%s of content in a %sx%s viewport"
      % (lay.get("scrollWidth"), lay.get("scrollHeight"),
         lay.get("innerWidth"), lay.get("innerHeight")))
print("browser test passed")
