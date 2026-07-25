// End-to-end browser check: drives the real page the way a user would --
// picks a file, winds in both thread modes, and reports what came out.
// Injected into a copy of index.html by run.sh; results are POSTed back.

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function makeSourceFile() {
  const c = document.createElement("canvas");
  c.width = c.height = 200;
  const g = c.getContext("2d");
  g.fillStyle = "#ffffff";
  g.fillRect(0, 0, 200, 200);
  g.fillStyle = "#101010";
  g.beginPath();
  g.arc(100, 100, 60, 0, Math.PI * 2);
  g.fill();
  g.fillStyle = "#cc2020";
  g.fillRect(24, 24, 46, 46);
  return new Promise((resolve) =>
    c.toBlob((b) => resolve(new File([b], "target.png", { type: "image/png" })), "image/png")
  );
}

function darkFraction(canvas) {
  const px = canvas.getContext("2d").getImageData(0, 0, canvas.width, canvas.height).data;
  let dark = 0;
  for (let i = 0; i < px.length; i += 4) if (px[i] < 200) dark++;
  return dark / (px.length / 4);
}

async function wind(mode) {
  const t0 = Date.now();
  document.getElementById("mode").value = mode;
  document.getElementById("run").click();
  for (let i = 0; i < 200 && document.getElementById("status").textContent !== "Done."; i++) {
    await sleep(100);
  }
  const svg = document.getElementById("dl-svg").getAttribute("href") || "";
  return {
    mode,
    ms: Date.now() - t0,
    status: document.getElementById("status").textContent,
    chords: parseInt(document.getElementById("stat-chords").textContent, 10),
    cuts: document.getElementById("stat-chords").textContent,
    thread: document.getElementById("stat-thread").textContent,
    match: parseFloat(document.getElementById("stat-match").textContent),
    dark: darkFraction(document.getElementById("result")),
    svgLines: (decodeURIComponent(svg).match(/<line /g) || []).length,
    seq: (document.getElementById("dl-seq").getAttribute("href") || "").length,
    png: document.getElementById("result").toDataURL("image/png"),
  };
}

(async () => {
  const report = { ok: false, error: null, runs: [] };
  try {
    const dt = new DataTransfer();
    dt.items.add(await makeSourceFile());
    const input = document.getElementById("file");
    input.files = dt.files;
    input.dispatchEvent(new Event("change"));
    for (let i = 0; i < 100 && document.getElementById("status").textContent.indexOf("Ready") < 0; i++) {
      await sleep(50);
    }
    document.getElementById("lines").value = "600";
    document.getElementById("size").value = "180";
    document.getElementById("pins").value = "160";
    report.runs.push(await wind("grayscale"));
    report.runs.push(await wind("colour"));
    report.ok = true;
  } catch (e) {
    report.error = String((e && e.stack) || e);
  }
  await fetch("/result", { method: "POST", body: JSON.stringify(report) });
})();
