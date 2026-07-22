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
dune exec bin/main.exe -- input.ppm -o out.ppm --color --lines 4000
```

PPM keeps the CLI free of an image codec dependency; the web app decodes
whatever the browser can open.

## Tests

```sh
dune test              # 93 unit and property tests over the core
./test/browser/run.sh  # drives the real page in headless Chrome, both modes
```

The browser test needs Google Chrome (override the path with `CHROME=`); it
skips itself if Chrome is not installed. Set `SELFTEST_PNG=/some/prefix` to have
it write out the canvases it produced.

## How it works

The board is white and thread is subtractive, so overlapping threads *multiply*
transmittances. Everything is therefore done in **optical density**
(`-log reflectance`), where stacking thread *adds* — which is what makes the
problem linear and the solver simple.

| | |
|---|---|
| `lib/image.ml` | linear-light RGB images, sRGB conversion, resampling |
| `lib/geometry.ml` | the pin frame |
| `lib/raster.ml` | anti-aliased line sampling, allocation-free |
| `lib/palette.ml` | threads as normalised density vectors |
| `lib/solver.ml` | greedy chord selection |
| `lib/render.ml` | wound sequence back to a picture |
| `lib/svg.ml`, `lib/ppm.ml` | exports |

Two consequences worth knowing:

- **Grayscale is not a special case.** It is the colour solver run on a
  desaturated target with a single black thread, so there is one code path.
- **Colour is one coupled solve**, not three independent channels. Each thread
  is a density vector; a chord is scored by how well that vector matches the
  residual, so the channels agree on where thread goes instead of fringing.

### The solver

At each step it looks at every chord leaving the current pin, admits the ones
that would actually reduce the error, and among those takes the one whose
thread colour best lines up with the residual — the standard matching-pursuit
criterion, `<r,d>²/‖d‖²`. One traversal per chord scores all threads at once.

The two halves of that rule do different jobs. Ranking on raw error reduction
instead would pick whichever thread has the largest density vector — always
black, since `‖d‖ = √3` against roughly 1 for the others — and the picture comes
out grey no matter what colour the subject is. Keeping the error reduction as
the *admission* test is what lets the solver stop on its own when no chord is
worth winding.

Its known, deliberate limits:

- myopic — it never removes a chord it has already placed;
- blind to cost — a chord twice as long counts the same as a short one;
- colour needs thread. A saturated subject wants far more chords than a
  grayscale one, because each thread only blocks one part of the spectrum;
  under-run, it comes out pale.

## Deliberately not done yet

Thread minimisation. The interesting question — *how little thread can you use
and still have it look the same?* — needs length-weighted scoring, a penalised
sparse solve to trace the fidelity/metres Pareto front, backward elimination,
perceptual rather than L2 error, and Eulerian repair so a selected chord *set*
can still be wound as one thread. None of that is here. This baseline exists to
be the thing all of it gets measured against.
