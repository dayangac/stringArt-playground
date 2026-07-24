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
dune exec bin/main.exe -- input.ppm -o out.ppm --colors 6 --board '#e8c9a0' 
```

PPM keeps the CLI free of an image codec dependency; the web app decodes
whatever the browser can open.

## Tests

```sh
dune test              # 119 unit and property tests over the core
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

## Deliberately not done yet

Thread minimisation. The interesting question — *how little thread can you use
and still have it look the same?* — needs length-weighted scoring, a penalised
sparse solve to trace the fidelity/metres Pareto front, backward elimination,
perceptual rather than L2 error, and Eulerian repair so a selected chord *set*
can still be wound as one thread. None of that is here. This baseline exists to
be the thing all of it gets measured against.
