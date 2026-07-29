# String Art

Turns a photograph into string art: one continuous thread wound between pins on
a circular frame, in black or in coloured thread. Written in OCaml, running in
the browser via `js_of_ocaml`.

This is the **baseline**. It solves the picture, it does not yet economise on
thread — see [Deliberately not done yet](#deliberately-not-done-yet).

## Running it

```sh
opam install js_of_ocaml js_of_ocaml-compiler brr alcotest qcheck qcheck-alcotest
dune build @default
python3 -m http.server -d _build/default/web 8000   # then open localhost:8000
```

Command line, for batch work and for looking at numbers:

```sh
dune exec bin/main.exe -- input.ppm -o out.ppm --lines 2500 --svg out.svg --seq winding.tsv
dune exec bin/main.exe -- input.ppm -o out.ppm --colors 6 --auto-board --prune 0.01
dune exec bin/bench.exe -- input.ppm            # what each thread-saving lever costs and buys
```

PPM keeps the CLI free of an image codec dependency; the web app decodes
whatever the browser can open.

## Tests

```sh
dune test              # 168 unit and property tests over the core
./test/browser/run.sh  # drives the real page in headless Chrome, both modes
```

The browser test needs Google Chrome (override the path with `CHROME=`); it
skips itself if Chrome is not installed. Set `SELFTEST_PNG=/some/prefix` to have
it write out the canvases it produced.

## How it works

Thread is **opaque**, not a filter. An orange thread on a white board makes an
orange line; it does not turn the white underneath orange the way ink would. So
a crossing takes over a fraction of the pixel,

```
C  <-  C + a*w*(thread - C)
```

and colours mix optically at viewing distance. This is the whole reason colour
works: the reachable colours are what you can mix out of the board and the
palette, and overshooting a channel costs error straight away.

Treating thread as subtractive ink with fixed CMY+K primaries instead — the
obvious first guess — makes every subject come out muddy grey-green, because
CMY only combines correctly when the layers are transparent.

| | |
|---|---|
| `lib/image.ml` | linear-light RGB images, sRGB conversion, resampling |
| `lib/geometry.ml` | the pin frame |
| `lib/raster.ml` | anti-aliased line sampling, allocation-free |
| `lib/oklab.ml` | perceptually uniform colour space |
| `lib/kmeans.ml` | deterministic clustering, seeded |
| `lib/palette.ml` | threads, and the palette clustered out of the picture |
| `lib/solver.ml` | greedy chord selection |
| `lib/render.ml` | wound sequence back to a picture |
| `lib/metrics.ml` | viewing-distance blur, SSIM, OKLab delta-E |
| `lib/descent.ml` | convex coordinate descent, one thread colour |
| `lib/surrogate.ml` | projected gradient over the colour surrogate |
| `lib/prune.ml` | backward elimination to a fidelity budget |
| `lib/sequence.ml` | continuity repair: a chord set back into a winding |
| `lib/wind.ml` | picks the solver and runs solve, prune, sequence |
| `lib/svg.ml`, `lib/ppm.ml` | exports |

Two consequences worth knowing:

- **Grayscale is not a special case.** It is the colour solver run on a
  desaturated target with a single black thread, so there is one code path.
- **Colour is one coupled solve**, not three independent channels. A chord is
  scored by what laying it would do to the whole pixel, so the channels agree on
  where thread goes instead of fringing.
- **The palette comes from your picture.** k-means in OKLab over the pixels
  inside the frame, so a fox comes out fox-coloured. Near-identical threads are
  merged; the board colour is yours to choose.

### The solver

At each step it looks at every chord leaving the current pin, takes the one that
reduces the error most, lays it down and moves on, stopping when nothing helps.
Expanding the error change leaves five sums that depend on the chord but not on
the colour, so **one traversal per chord scores every thread at once**.

Scoring walks every other pixel and ignores anti-aliasing; whatever wins is then
laid down over every pixel, anti-aliased, with the error kept exact. Measured on
a photographic target at 1500 chords, that is 1.9× faster for 0.14 points of the
target explained.

Its known, deliberate limits:

- myopic — it never removes a chord it has already placed;
- blind to cost — a chord twice as long counts the same as a short one;
- the walk is one thread that changes colour, rather than one continuous thread
  per colour, so it is not yet a practical winding plan for a colour piece.

## Using less thread

The goal is to use as little thread as possible while the picture still looks
the same, so "looks the same" is a measured number, not an opinion: SSIM on
OKLab lightness plus mean OKLab delta-E, both taken through a blur derived from
one arcminute of acuity at the intended viewing distance.

`dune exec bin/bench.exe -- image.ppm` runs every lever against the baseline.
On a fox-like target, 220px working resolution, 180 pins, 1500 chords, six
colours, a 0.6 m frame seen from 2 m:

| variant | metres | vs base | ssim | deltaE | cuts |
|---|---|---|---|---|---|
| baseline | 723.9 | — | 0.412 | 0.132 | 0 |
| per-length ranking | 642.7 | −11.2% | 0.386 | 0.145 | 0 |
| perceptual weighting | 722.2 | −0.2% | 0.315 | 0.138 | 0 |
| board matched to image | 686.1 | −5.2% | **0.693** | **0.066** | 0 |
| chord reuse (×3) | 723.9 | 0.0% | 0.412 | 0.132 | 0 |
| all levers | 656.9 | −9.3% | 0.594 | 0.066 | 0 |
| all + prune 0.03 | **612.6** | **−15.4%** | 0.564 | 0.068 | 40 |

So: **15% less thread, and it looks better than the baseline on both measures**
— SSIM 0.56 against 0.41, colour error halved. The catch is 40 cuts, and that
matching the board means buying or painting a board that colour.

What each lever actually does, including where it does nothing:

- **Board matched to the image** is the one that matters, and it is barely an
  algorithm. Thread only has to cover what differs from the board, so a large
  flat background is free if the board already is that colour. Choosing it has
  to weigh *coverage*, not just closeness: scoring by distance alone hands a
  single black thread a black board and leaves it nothing to do.
- **Per-length ranking** trades fidelity for thread rather than giving it away:
  −11% thread, but SSIM falls too. A real Pareto move, not a free win.
- **Pruning** drops the chords that stopped paying, ranked by what each one
  actually bought, binary searching on how many can go before the picture
  changes. The budget bounds SSIM *and* colour: SSIM alone is not enough,
  because on a nearly flat picture a bare board scores above 0.9 against the
  target and a pruner given only an SSIM budget hands back an empty frame.
- **Perceptual weighting** did not pay off here and is off by default. It is
  available (`--economise`) but unvalidated.
- **Chord reuse** changed nothing: greedy never wanted to wind a chord twice at
  these budgets.

One measurement worth recording: at 220px the viewing blur is inactive. A 0.6 m
frame at 2 m gives a sigma of 0.11 px, and it only reaches 1 px at about 19 m —
the working resolution is already coarser than the eye. So the lever there is
*solving smaller*, not blurring the metric.

## Three solvers

`Wind.solve` picks one and then runs the same pipeline: **solve, prune, sequence** —
in that order, because pruning ranks chords by what each bought during the solve
and knows nothing about the repair chords the sequencer invents.

| | | |
|---|---|---|
| `greedy` | the unoptimised baseline | one continuous walk, no knob but stopping early |
| `descent` | exactly convex, single colour | `lambda` is the price of thread |
| `surrogate` | projected gradient, colour | `lambda` is the price of thread |

Switch with `--algorithm` on the CLI, the **Solver** dropdown on the page, or
`Wind.solve ~algorithm`. `bin/bench.exe` runs them all side by side.

**The baseline is never more than a click away.** `Solver.default_config` has
every lever off and `Wind.Greedy` is the default; a test pins both, field by
field, so flipping a default has to break something. The page ships with the
economy levers on, so it also carries a *Reset to the unoptimised baseline*
button, and the browser test checks that it really does reset them.

Grayscale, 180px, 140 pins, 1200 chords, 0.6 m frame at 2 m:

| variant | metres | vs base | ssim | deltaE | cuts |
|---|---|---|---|---|---|
| baseline | 516.2 | — | 0.202 | 0.089 | 0 |
| all levers + prune 0.03 | 436.8 | −15.4% | 0.197 | 0.107 | 57 |
| descent, lambda 0 | 494.7 | −4.2% | 0.256 | 0.110 | 0 |
| descent, lambda 0.1 | 429.8 | −16.7% | 0.261 | 0.123 | 0 |
| descent, lambda 0.25 | **365.2** | **−29.3%** | 0.264 | 0.142 | 0 |

Descent is the only thing here with a real dial: greedy can only use less
thread by stopping early, whereas `lambda` prices thread inside the objective
and lets it put a chord *back*. It buys structure and costs a little colour
accuracy — reported as it is, not as a win. The surrogate does the same for
colour but did not beat greedy on the test target (662.6 m against 578.5), and
that is in the bench for anyone who wants to check.

Both return a chord *set*, so `Sequence` makes it windable again: join the
pieces with the cheapest chords that link them, then fix parities within each
piece. It reports the repair thread and the tie-offs separately rather than
folding them into the total. Colours are wound as separate strands, which is
why the colour baseline honestly shows four tie-offs for a five-colour palette.

## Deliberately not done yet

- **Parameters as decision variables** — pin count, thread opacity and palette
  size are all levers with real thread consequences, and all are still fixed by
  hand rather than swept and chosen.
- **Spatial coherence in colour.** Optical mixing means interleaved colours read
  as speckle; a term preferring few colours per region should raise *perceived*
  quality at the same thread.
- **A palette chosen for economy.** k-means minimises colour distortion, not the
  coverage the palette will cost.
- **Layer order.** With per-colour strands the only cross-colour freedom left is
  the order they go down in, which for six colours is small enough to search
  exhaustively by exact replay.
- **The non-greedy solvers in the browser.** They are a second or two natively
  and correspondingly slower in JS; the page offers them but greedy is the
  sensible default there.
