/*
 * The winding as a physical object.
 *
 * The flat render answers "what will it look like". This answers "what is
 * actually there": every chord as a separate strand at its own height, in the
 * order it was wound, so the pile is visible. That pile is the whole reason
 * the model is what it is -- opaque thread does not commute, and here you can
 * see why: the strands laid last are the ones on top.
 *
 * All the chords live in one LineSegments buffer, so a winding of several
 * thousand strands is a single draw call and the scrubber is just a draw
 * range rather than a rebuild.
 */
import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";

/** What the OCaml side leaves on the window after a wind. */
export interface Winding {
  pins: number;
  palette: string[];
  board: string;
  /** chord endpoints and thread colour, one entry per chord, in winding order */
  a: number[];
  b: number[];
  thread: number[];
}

declare global {
  interface Window {
    __stringart?: Winding;
    __stringartView?: { rebuild: () => void; shown: () => number };
  }
}

const RADIUS = 1;
/** How tall the pile of thread stands, relative to the frame's radius. */
const STACK = 0.07;
const PIN_HEIGHT = 0.05;

function pinAt(i: number, pins: number): THREE.Vector3 {
  const t = (2 * Math.PI * i) / pins;
  return new THREE.Vector3(RADIUS * Math.cos(t), RADIUS * Math.sin(t), 0);
}

export class View {
  private renderer: THREE.WebGLRenderer;
  private scene = new THREE.Scene();
  private camera: THREE.PerspectiveCamera;
  private controls: OrbitControls;
  private chords: THREE.LineSegments | null = null;
  private pinMesh: THREE.InstancedMesh | null = null;
  private board: THREE.Mesh | null = null;
  private total = 0;
  private visible = 0;

  constructor(private host: HTMLElement) {
    this.renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    host.appendChild(this.renderer.domElement);
    this.renderer.domElement.style.width = "100%";
    this.renderer.domElement.style.height = "100%";
    this.renderer.domElement.style.display = "block";

    this.camera = new THREE.PerspectiveCamera(38, 1, 0.01, 100);
    this.camera.position.set(0, -2.6, 1.9);
    this.camera.up.set(0, 0, 1);

    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableDamping = true;
    this.controls.target.set(0, 0, STACK / 2);

    this.scene.add(new THREE.AmbientLight(0xffffff, 1.6));
    const key = new THREE.DirectionalLight(0xffffff, 1.5);
    key.position.set(2, -3, 4);
    this.scene.add(key);

    this.resize();
    window.addEventListener("resize", () => this.resize());
    const tick = () => {
      this.controls.update();
      this.renderer.render(this.scene, this.camera);
      requestAnimationFrame(tick);
    };
    tick();
  }

  resize(): void {
    const w = this.host.clientWidth || 1;
    const h = this.host.clientHeight || 1;
    this.renderer.setSize(w, h, false);
    this.camera.aspect = w / h;
    this.camera.updateProjectionMatrix();
  }

  /** How many chords are currently drawn, for the scrubber and for tests. */
  shown(): number {
    return this.visible;
  }

  chordCount(): number {
    return this.total;
  }

  /** Draw only the first n chords, so the winding can be watched in order. */
  reveal(n: number): void {
    this.visible = Math.max(0, Math.min(this.total, Math.floor(n)));
    if (this.chords) this.chords.geometry.setDrawRange(0, this.visible * 2);
  }

  private clear(): void {
    for (const o of [this.chords, this.pinMesh, this.board]) {
      if (!o) continue;
      this.scene.remove(o);
      o.geometry.dispose();
      const m = o.material;
      if (Array.isArray(m)) m.forEach((x) => x.dispose());
      else m.dispose();
    }
    this.chords = null;
    this.pinMesh = null;
    this.board = null;
  }

  build(w: Winding): void {
    this.clear();
    const n = Math.min(w.a.length, w.b.length, w.thread.length);
    this.total = n;

    // the board the thread is wound on, sitting just under everything
    const board = new THREE.Mesh(
      new THREE.CircleGeometry(RADIUS * 1.04, 128),
      new THREE.MeshBasicMaterial({ color: new THREE.Color(w.board) })
    );
    board.position.z = -0.005;
    this.board = board;
    this.scene.add(board);

    // one pin per position, as an instanced cylinder: hundreds of them at the
    // cost of one
    const pin = new THREE.InstancedMesh(
      new THREE.CylinderGeometry(0.006, 0.006, PIN_HEIGHT, 8),
      new THREE.MeshLambertMaterial({ color: 0x8a8178 }),
      w.pins
    );
    const m = new THREE.Matrix4();
    const upright = new THREE.Matrix4().makeRotationX(Math.PI / 2);
    for (let i = 0; i < w.pins; i++) {
      const p = pinAt(i, w.pins);
      m.copy(upright).setPosition(p.x, p.y, PIN_HEIGHT / 2);
      pin.setMatrixAt(i, m);
    }
    pin.instanceMatrix.needsUpdate = true;
    this.pinMesh = pin;
    this.scene.add(pin);

    // every chord in one buffer, coloured per vertex, lifted by its place in
    // the winding order so the strands stack instead of fighting for z
    const pos = new Float32Array(n * 6);
    const col = new Float32Array(n * 6);
    const colours = w.palette.map((hex) => new THREE.Color(hex));
    for (let i = 0; i < n; i++) {
      const z = n > 1 ? (i / (n - 1)) * STACK : 0;
      const p = pinAt(w.a[i], w.pins);
      const q = pinAt(w.b[i], w.pins);
      pos.set([p.x, p.y, z, q.x, q.y, z], i * 6);
      const c = colours[w.thread[i]] ?? new THREE.Color(0x222222);
      col.set([c.r, c.g, c.b, c.r, c.g, c.b], i * 6);
    }
    const geom = new THREE.BufferGeometry();
    geom.setAttribute("position", new THREE.BufferAttribute(pos, 3));
    geom.setAttribute("color", new THREE.BufferAttribute(col, 3));
    const lines = new THREE.LineSegments(
      geom,
      new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 0.85 })
    );
    this.chords = lines;
    this.scene.add(lines);
    this.reveal(n);
  }
}

function mount(): void {
  const host = document.getElementById("three-host");
  const scrub = document.getElementById("three-scrub") as HTMLInputElement | null;
  const note = document.getElementById("three-note");
  if (!host) return;
  const view = new View(host);

  const rebuild = (): void => {
    const w = window.__stringart;
    if (!w || w.a.length === 0) {
      if (note) note.textContent = "Wind something first, then come back.";
      return;
    }
    view.build(w);
    if (scrub) {
      scrub.max = String(w.a.length);
      scrub.value = String(w.a.length);
    }
    if (note) note.textContent = `${w.a.length} strands, laid bottom to top in winding order.`;
    view.resize();
  };

  scrub?.addEventListener("input", () => {
    view.reveal(Number(scrub.value));
    if (note) note.textContent = `${view.shown()} of ${view.chordCount()} strands.`;
  });

  window.__stringartView = { rebuild, shown: () => view.shown() };
  rebuild();
}

if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", mount);
else mount();
