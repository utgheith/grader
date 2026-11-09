// The run graph's viewer, made into one page with graph.html and graph.css
// by ag.r2.RunGraph. GRAPH, from data.js, is what a run did: every target it
// used, how each got its value, what each depends on, and the commands that
// ran to compute it.
//
// Served by ag.r2.WebServer, the page is live instead (GRAPH is null): it
// gets the graph from the server's event stream as the run goes, along with
// the run's controls (pause, breakpoints, held targets).
(() => {
  "use strict";

  const LIVE = typeof GRAPH === "undefined" || GRAPH === null;
  const $ = (id) => document.getElementById(id);

  /////////////// the data ///////////////

  let G = null;               // the graph, as GRAPH
  let ROOT = "(run)";
  let by_path = new Map(), kinds = new Map(), all_commands = [];
  // each target's depth in the whole run (see depths): every view places
  // targets by it, so they read the same way, in the same order, everywhere
  let depth = new Map();
  // live only: the run's controls, and the connection to the server
  let control = { paused: false, break_on_failure: false, breakpoints: [], waiting: [] };
  let held = new Map();       // path -> the held entry for it
  let connection = LIVE ? "connecting" : "static";

  const STATUSES = ["ran", "failed", "blocked", "used", "reused", "root"];
  const label = (s) => ({
    ran: "ran", reused: "reused", failed: "failed", blocked: "failed (dependency)",
    used: G && !G.done ? "running" : "never finished", root: "the run",
  })[s];

  // a definition's short name: its last two parts, e.g. Project/run_one
  const short_kind = (k) => k === ROOT ? ROOT : k.split("/").slice(-2).join("/");
  // what tells a target apart from the kind's other targets
  const params = (n) => n.path === n.kind ? "" :
    n.path.startsWith(n.kind + "/") ? n.path.slice(n.kind.length + 1) : n.path;
  // a kind's color: the most telling of its targets' outcomes
  const kind_status = (k) =>
    ["failed", "blocked", "ran", "used", "root"].find((s) => k.counts[s]) || "reused";

  // works out what the views need from G; again after every change
  function index_data() {
    ROOT = G.root;
    by_path = new Map();
    for (const n of G.nodes) {
      n.dependents = [];
      by_path.set(n.path, n);
    }
    for (const n of G.nodes) {
      n.changed_set = new Set(n.changed);
      for (const [d, peek] of n.deps) {
        const m = by_path.get(d);
        if (m) m.dependents.push([n.path, peek]);
      }
    }
    // a failure is "blocked" if it only passed on a dependency's failure
    for (const n of G.nodes) {
      if (n.path === ROOT) n.status = "root";
      else if (n.outcome === "failed")
        n.status = n.deps.some(([d, peek]) => !peek && by_path.get(d)?.outcome === "failed")
          ? "blocked" : "failed";
      else n.status = n.outcome;
      n.total = n.start != null && n.end != null ? n.end - n.start : null;
      n.own = n.run_start != null && n.run_end != null ? n.run_end - n.run_start : null;
    }
    kinds = new Map();
    for (const n of G.nodes) {
      let k = kinds.get(n.kind);
      if (!k) {
        k = { id: n.kind, nodes: [], counts: {}, deps: new Map(), own: 0 };
        kinds.set(n.kind, k);
      }
      k.nodes.push(n);
      k.counts[n.status] = (k.counts[n.status] || 0) + 1;
      k.own += n.own || 0;
    }
    for (const n of G.nodes) {
      const k = kinds.get(n.kind);
      for (const [d, peek] of n.deps) {
        const dk = by_path.get(d)?.kind;
        if (dk == null || dk === n.kind) continue;
        const e = k.deps.get(dk) || { n: 0, peek: true };
        e.n += 1;
        e.peek = e.peek && peek;
        k.deps.set(dk, e);
      }
    }
    all_commands = G.nodes.flatMap((n) => n.commands.map((c) => ({ ...c, node: n })));
    depth = depths(G.nodes.map((n) => n.path), (p) => by_path.get(p).deps.map(([d]) => d).filter((d) => by_path.has(d)));
    held = new Map(control.waiting.map((w) => [w.path, w]));
  }

  /////////////// helpers ///////////////

  const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
  const ms = (t) => {
    if (t == null) return "";
    if (t < 1000) return `${t} ms`;
    if (t < 60000) return `${(t / 1000).toFixed(1)} s`;
    const m = Math.floor(t / 60000);
    return `${m}m ${Math.round((t % 60000) / 1000)}s`;
  };
  const at = (t) => t == null ? "" : `+${ms(t)}`;
  const shell = (cmd) => cmd.map((a) => /^[\w@%+=:,./-]+$/.test(a) ? a : `'${a.replace(/'/g, `'\\''`)}'`).join(" ");
  const middle = (s, max) => s.length <= max ? s : s.slice(0, Math.ceil(max / 2) - 1) + "…" + s.slice(s.length - Math.floor(max / 2));
  const href = (view, args = {}) => {
    const p = new URLSearchParams();
    p.set(view, args[view] ?? "");
    for (const [k, v] of Object.entries(args)) if (k !== view && v != null && v !== "") p.set(k, v);
    return "#" + p.toString().replace(/=(&|$)/, "$1");
  };
  const target_link = (path, text) =>
    `<a href="${esc(href("t", { t: path }))}">${esc(text ?? path)}</a>`;
  const badge = (status) => `<span class="badge st-${status}">${esc(label(status))}</span>`;

  /////////////// completion ///////////////

  // How what's typed can go on, for the boxes that find targets by part of
  // their path: each way to extend it, up to the next "/", with how many
  // targets have it. Matches start at a segment, so "run_o" finds
  // ag/grader/Project/run_one/...; "" marks targets that end right there.
  function continuations(typed) {
    const t = typed.toLowerCase();
    const next = new Map();
    for (const n of G ? G.nodes : []) {
      const p = n.path, lp = p.toLowerCase();
      const seen = new Set();
      for (let i = lp.indexOf(t); i >= 0; i = t ? lp.indexOf(t, i + 1) : -1) {
        if (i !== 0 && p[i - 1] !== "/") continue;
        const rest = p.slice(i + t.length);
        const slash = rest.indexOf("/");
        const piece = slash < 0 ? rest : rest.slice(0, slash + 1);
        if (seen.has(piece)) continue;
        seen.add(piece);
        next.set(piece, (next.get(piece) || 0) + 1);
      }
    }
    return next;
  }

  const common_prefix = (xs) => xs.length ? xs.reduce((a, x) => {
    let i = 0;
    while (i < a.length && i < x.length && a[i].toLowerCase() === x[i].toLowerCase()) i++;
    return a.slice(0, i);
  }) : "";

  // Shell-style Tab completion for `input`: Tab extends what's typed as far
  // as every match agrees; when there's nothing to add, it lists the ways on
  // (arrows and Enter, or a click, pick one; Escape closes the list).
  const menu = document.createElement("ul");
  menu.className = "completions";
  menu.hidden = true;
  document.body.append(menu);
  let menu_for = null;
  const close_menu = () => { menu.hidden = true; menu_for = null; };
  menu.addEventListener("mousedown", (e) => e.preventDefault());  // keep the focus
  menu.addEventListener("click", (e) => {
    const li = e.target.closest("li[data-v]");
    if (li && menu_for) { menu_for.value = li.dataset.v; menu_for.focus(); close_menu(); }
  });
  window.addEventListener("resize", close_menu);

  function complete_in(input) {
    input.addEventListener("keydown", (e) => {
      const open = !menu.hidden && menu_for === input;
      if (open && (e.key === "ArrowDown" || e.key === "ArrowUp")) {
        e.preventDefault();
        const items = [...menu.children];
        const i = items.findIndex((li) => li.classList.contains("on"));
        const down = e.key === "ArrowDown";
        const j = i < 0 ? (down ? 0 : items.length - 1) : (i + (down ? 1 : -1) + items.length) % items.length;
        items.forEach((li, k) => li.classList.toggle("on", k === j));
        items[j].scrollIntoView({ block: "nearest" });
        return;
      }
      if (open && e.key === "Enter") {
        const on = menu.querySelector("li.on");
        if (on) { e.preventDefault(); input.value = on.dataset.v; close_menu(); return; }
      }
      if (e.key === "Escape" && open) { e.stopPropagation(); close_menu(); return; }
      if (e.key !== "Tab" || e.shiftKey || e.altKey || e.ctrlKey || e.metaKey) return;
      e.preventDefault();
      const typed = input.value;
      const next = continuations(typed);
      const pieces = [...next.keys()].filter((p) => p !== "");
      if (!pieces.length) { close_menu(); input.classList.add("nomatch"); setTimeout(() => input.classList.remove("nomatch"), 400); return; }
      const common = common_prefix(pieces);
      if (common) {
        // the next Tab lists what's left, if anything is
        input.value = typed + common;
        close_menu();
        return;
      }
      // list them, with how many targets each leads to
      const base = input.value;
      const choices = [...continuations(base)].sort((a, b) => a[0].localeCompare(b[0])).slice(0, 200);
      menu.innerHTML = choices.map(([piece, count]) =>
        `<li data-v="${esc(base + piece)}"><span>${esc(base)}<b>${esc(piece) || "<i>(ends here)</i>"}</b></span><span class="n">${count}</span></li>`).join("");
      const r = input.getBoundingClientRect();
      menu.style.left = `${r.left}px`;
      menu.style.top = `${r.bottom + 2}px`;
      menu.style.minWidth = `${r.width}px`;
      menu.hidden = false;
      menu_for = input;
    });
    input.addEventListener("input", () => { if (menu_for === input) close_menu(); });
    input.addEventListener("blur", () => { if (menu_for === input) close_menu(); });
  }

  /////////////// layout ///////////////

  // Layered layout, dependencies to the left of what uses them. `ids` are the
  // nodes to show, deps(id) the ids it depends on (others are ignored), and
  // size(id) its [width, height]. Tall columns wrap, at more rows for bigger
  // graphs, to keep them from getting much wider than they're tall.
  const ROW_GAP = 10, COL_GAP = 50, LAYER_GAP = 120;
  // The longest chain of dependencies below each of `ids`, following
  // preds(id) (all of them in `ids`): 0 for what depends on nothing.
  function depths(ids, preds) {
    const succs = new Map(ids.map((id) => [id, []]));
    const indeg = new Map(), out = new Map();
    for (const id of ids) {
      const ps = preds(id);
      indeg.set(id, ps.length);
      out.set(id, 0);
      for (const p of ps) succs.get(p).push(id);
    }
    const queue = ids.filter((id) => indeg.get(id) === 0);
    for (let i = 0; i < queue.length; i++) {
      const id = queue[i];
      for (const s of succs.get(id)) {
        out.set(s, Math.max(out.get(s), out.get(id) + 1));
        indeg.set(s, indeg.get(s) - 1);
        if (indeg.get(s) === 0) queue.push(s);
      }
    }
    return out;
  }

  // `rank(id)`, if given, orders the columns instead of the depths among
  // `ids`: what's shown of a bigger graph keeps that graph's order (columns
  // no shown node needs are dropped).
  function layout(ids, deps, size, rank) {
    const MAX_ROWS = Math.max(40, Math.ceil(3 * Math.sqrt(ids.length)));
    const shown = new Set(ids);
    const preds = new Map(), succs = new Map();
    for (const id of ids) { preds.set(id, []); succs.set(id, []); }
    for (const id of ids)
      for (const d of deps(id))
        if (shown.has(d) && d !== id) { preds.get(id).push(d); succs.get(d).push(id); }

    // columns: by rank, else by depth among the shown nodes
    let layer;
    if (rank) {
      const column = new Map([...new Set(ids.map(rank))].sort((a, b) => a - b).map((r, i) => [r, i]));
      layer = new Map(ids.map((id) => [id, column.get(rank(id))]));
    } else layer = depths(ids, (id) => preds.get(id));
    const layers = [];
    for (const id of ids) (layers[layer.get(id)] ||= []).push(id);
    for (let i = 0; i < layers.length; i++) layers[i] ||= [];

    // order within layers: a few sweeps of the barycenter heuristic
    const pos = new Map();
    const place = () => layers.forEach((l) => l.forEach((id, i) => pos.set(id, l.length > 1 ? i / (l.length - 1) : 0.5)));
    place();
    const sweep = (l, nbrs) => {
      const key = new Map(l.map((id) => {
        const ns = nbrs.get(id);
        return [id, ns.length ? ns.reduce((a, n) => a + pos.get(n), 0) / ns.length : pos.get(id)];
      }));
      l.sort((a, b) => key.get(a) - key.get(b));
    };
    for (let it = 0; it < 4; it++) {
      for (let i = 1; i < layers.length; i++) { sweep(layers[i], preds); place(); }
      for (let i = layers.length - 2; i >= 0; i--) { sweep(layers[i], succs); place(); }
    }

    // coordinates
    const at = new Map();
    let x = 0, height = 0;
    const columns = [];
    for (const l of layers) {
      if (!l.length) continue;
      const w = Math.max(...l.map((id) => size(id)[0]));
      for (let c = 0; c * MAX_ROWS < l.length; c++) {
        const col = l.slice(c * MAX_ROWS, (c + 1) * MAX_ROWS);
        const h = col.reduce((a, id) => a + size(id)[1] + ROW_GAP, -ROW_GAP);
        columns.push({ x, w, col, h });
        height = Math.max(height, h);
        x += w + COL_GAP;
      }
      x += LAYER_GAP - COL_GAP;
    }
    for (const { x, w, col, h } of columns) {
      let y = (height - h) / 2;
      for (const id of col) {
        const nh = size(id)[1];
        at.set(id, { x, y, w, h: nh });
        y += nh + ROW_GAP;
      }
    }
    return { at, preds, width: Math.max(0, x - LAYER_GAP), height };
  }

  /////////////// the graph widget ///////////////

  // where each view was panned and zoomed to, so a live update that draws it
  // again keeps it there
  const views = new Map();
  let dragging = false;

  // Draws a graph in `box`, which can be panned (drag), zoomed (wheel, the
  // buttons) and clicked. spec: ids, deps(id) -> [[dep, peek, changed]],
  // lines(id) -> [title, subtitle], status(id), held(id), bar(id) -> counts
  // or null, title(id), select(id), open(id), selected, key (for `views`),
  // rank(id) (see layout).
  function draw_graph(box, spec) {
    const CH = 6.6, PAD = 12;
    const lines = new Map(spec.ids.map((id) => [id, spec.lines(id)]));
    const sizes = new Map(spec.ids.map((id) => {
      const [a, b] = lines.get(id);
      const w = Math.min(330, Math.max(110, Math.max(a.length * 7, b.length * CH) + 2 * PAD));
      return [id, [w, b ? 38 : 26]];
    }));
    const edges_of = new Map(spec.ids.map((id) => [id, spec.deps(id)]));
    const L = layout(spec.ids, (id) => edges_of.get(id).map((e) => e[0]), (id) => sizes.get(id), spec.rank);

    const index = new Map(spec.ids.map((id, i) => [id, i]));
    const parts = [];
    const edge_list = [];
    for (const id of spec.ids) {
      const b = L.at.get(id);
      for (const [d, peek, changed] of edges_of.get(id)) {
        const a = L.at.get(d);
        if (!a || d === id) continue;
        // from the dependency's side facing its user, to the user's facing side
        const right = a.x < b.x;
        const x1 = right ? a.x + a.w : a.x, y1 = a.y + a.h / 2;
        const x2 = right ? b.x : b.x + b.w, y2 = b.y + b.h / 2;
        const dx = Math.max(40, Math.abs(x2 - x1) / 2) * (right ? 1 : -1);
        const cls = "edge" + (peek ? " peek" : "") + (changed ? " changed" : "");
        parts.push(`<path class="${cls}" d="M${x1},${y1} C${x1 + dx},${y1} ${x2 - dx},${y2} ${x2},${y2}"/>`);
        edge_list.push([index.get(d), index.get(id)]);
      }
    }
    spec.ids.forEach((id, i) => {
      const p = L.at.get(id);
      const [a, b] = lines.get(id);
      const max = Math.floor((p.w - 2 * PAD) / CH);
      let bar = "";
      const counts = spec.bar && spec.bar(id);
      if (counts) {
        const total = Object.values(counts).reduce((s, v) => s + v, 0);
        let bx = 0;
        for (const s of STATUSES) {
          if (!counts[s]) continue;
          const w = (p.w - 2) * counts[s] / total;
          bar += `<rect class="bar st-${s}" x="${1 + bx}" y="${p.h - 5}" width="${w}" height="4" style="fill:var(--c)"/>`;
          bx += w;
        }
      }
      const extra = spec.held && spec.held(id) ? " held" : "";
      parts.push(
        `<g class="node st-${spec.status(id)}${extra}" data-i="${i}" transform="translate(${p.x},${p.y})">` +
        `<rect width="${p.w}" height="${p.h}" rx="6"/>${bar}` +
        `<text x="${PAD}" y="${b ? 16 : 17}">${esc(middle(a, Math.floor((p.w - 2 * PAD) / 7)))}</text>` +
        (b ? `<text class="sub" x="${PAD}" y="${31}">${esc(middle(b, max))}</text>` : "") +
        `<title>${esc(spec.title(id))}</title></g>`);
    });

    box.innerHTML =
      `<svg xmlns="http://www.w3.org/2000/svg"><g class="vp">${parts.join("")}</g></svg>` +
      `<div class="zoom"><button data-z="in" title="Zoom in">+</button>` +
      `<button data-z="out" title="Zoom out">−</button>` +
      `<button data-z="fit" title="Fit (f)">⤢</button></div>`;
    const svg = box.querySelector("svg");
    const vp = svg.querySelector(".vp");
    const node_els = [...svg.querySelectorAll(".node")];
    const edge_els = [...svg.querySelectorAll(".edge")];

    // pan and zoom
    let view = { x: 0, y: 0, k: 1 };
    const apply = () => {
      vp.setAttribute("transform", `translate(${view.x},${view.y}) scale(${view.k})`);
      if (spec.key) views.set(spec.key, { ...view });
    };
    const fit = () => {
      const r = svg.getBoundingClientRect();
      const k = Math.min(1.2, Math.min(r.width / (L.width + 40), r.height / (L.height + 40)));
      view = { k, x: (r.width - L.width * k) / 2, y: (r.height - L.height * k) / 2 };
      apply();
    };
    const zoom_at = (f, mx, my) => {
      const k = Math.min(4, Math.max(0.01, view.k * f));
      view.x = mx - (mx - view.x) * (k / view.k);
      view.y = my - (my - view.y) * (k / view.k);
      view.k = k;
      apply();
    };
    const center_on = (i) => {
      const p = L.at.get(spec.ids[i]);
      const r = svg.getBoundingClientRect();
      view.k = Math.max(view.k, 0.8);
      view.x = r.width / 2 - (p.x + p.w / 2) * view.k;
      view.y = r.height / 2 - (p.y + p.h / 2) * view.k;
      apply();
    };
    svg.addEventListener("wheel", (e) => {
      e.preventDefault();
      const r = svg.getBoundingClientRect();
      zoom_at(Math.exp(-e.deltaY * (e.deltaMode ? 0.05 : 0.0015)), e.clientX - r.left, e.clientY - r.top);
    }, { passive: false });
    box.querySelector(".zoom").addEventListener("click", (e) => {
      const z = e.target.closest("button")?.dataset.z;
      const r = svg.getBoundingClientRect();
      if (z === "fit") fit();
      else if (z) zoom_at(z === "in" ? 1.4 : 1 / 1.4, r.width / 2, r.height / 2);
    });
    let drag = null;
    svg.addEventListener("pointerdown", (e) => {
      drag = { x: e.clientX, y: e.clientY, vx: view.x, vy: view.y, moved: false };
      dragging = true;
      svg.setPointerCapture(e.pointerId);
    });
    svg.addEventListener("pointermove", (e) => {
      if (!drag) return;
      const dx = e.clientX - drag.x, dy = e.clientY - drag.y;
      if (Math.abs(dx) + Math.abs(dy) > 4) { drag.moved = true; svg.classList.add("dragging"); }
      if (drag.moved) { view.x = drag.vx + dx; view.y = drag.vy + dy; apply(); }
    });
    const end_drag = (e) => {
      if (!drag) return;
      const moved = drag.moved;
      drag = null;
      dragging = false;
      svg.classList.remove("dragging");
      if (moved) return;
      const el = document.elementFromPoint(e.clientX, e.clientY)?.closest(".node");
      if (el) select(+el.dataset.i, true);
      else select(-1, true);
    };
    svg.addEventListener("pointerup", end_drag);
    svg.addEventListener("pointercancel", () => { drag = null; dragging = false; svg.classList.remove("dragging"); });
    svg.addEventListener("dblclick", (e) => {
      const el = e.target.closest(".node");
      if (el && spec.open) spec.open(spec.ids[+el.dataset.i]);
    });

    // selecting a node shows it with its neighbors
    function select(i, from_click) {
      node_els.forEach((el) => el.classList.remove("sel", "hl"));
      edge_els.forEach((el) => el.classList.remove("hl"));
      svg.classList.toggle("focus", i >= 0);
      if (i >= 0) {
        node_els[i].classList.add("sel");
        edge_list.forEach(([a, b], e) => {
          if (a === i || b === i) {
            edge_els[e].classList.add("hl");
            node_els[a === i ? b : a].classList.add("hl");
          }
        });
      }
      if (from_click && spec.select) spec.select(i >= 0 ? spec.ids[i] : null);
    }

    const saved = spec.key && views.get(spec.key);
    const i = spec.selected != null ? index.get(spec.selected) : undefined;
    if (saved) {
      view = { ...saved };
      apply();
      if (i != null) select(i, false);
    } else {
      requestAnimationFrame(() => {
        fit();
        if (i != null) {
          select(i, false);
          if (spec.ids.length > 30) center_on(i);
        }
      });
    }
    box.graph_keys = (e) => { if (e.key === "f") fit(); };
  }

  /////////////// the target graphs ///////////////

  const node_lines = (n) => n.path === ROOT ? [ROOT, ""] : [short_kind(n.kind), params(n)];
  const node_title = (n) => `${n.path}\n${label(n.status)}${n.reason ? ": " + n.reason : ""}` +
    (held.has(n.path) ? `\nheld at ${held.get(n.path).gate}` : "") +
    (n.own != null ? `\nown time ${ms(n.own)}` : "") +
    (n.commands.length ? `\n${n.commands.length} command(s)` : "");

  // How the target graphs order their columns, left to right (the viewer's
  // choice, remembered in this browser):
  //   deps:   by depth in the whole run; what a target depends on is to its left
  //   ready:  by when each got its value; still left of what uses it, since
  //           nothing is ready before what it depends on
  //   needed: by when each was first needed, a timeline; what uses a target is
  //           mostly to its left, except shared ones needed earlier
  const ORDERS = { deps: "dependencies", ready: "ready at", needed: "needed at" };
  let order = "deps";
  try { order = localStorage.getItem("graph_order") || "deps"; } catch (e) {}
  if (!ORDERS[order]) order = "deps";

  function order_select() {
    return `<label>order <select id="order">${Object.entries(ORDERS).map(([k, v]) =>
      `<option value="${k}" ${k === order ? "selected" : ""}>${v}</option>`).join("")}</select></label>` +
      (order === "deps" ? "" : `<span class="note">earliest on the left</span>`);
  }
  function watch_order() {
    $("order").onchange = (e) => {
      order = e.target.value;
      try { localStorage.setItem("graph_order", order); } catch (err) {}
      views.delete(location.hash);  // a different layout: fit it anew
      route();
    };
  }

  // Columns for `paths` in time order: each starts when the current one is
  // full, or holds something linked to the next target, so edges never run
  // within a column (and, ordered by ready at, all point right).
  function time_columns(paths) {
    const BIG = Number.MAX_SAFE_INTEGER;
    const time = (p) => {
      const n = by_path.get(p);
      if (order === "ready") return n.path === ROOT ? BIG : n.end ?? BIG - 1;  // unfinished last
      return n.path === ROOT ? -1 : n.start ?? BIG;
    };
    const shown = new Set(paths);
    const linked = (p) => {
      const n = by_path.get(p);
      return [...n.deps, ...n.dependents].map(([q]) => q).filter((q) => shown.has(q));
    };
    const cap = Math.max(40, Math.ceil(3 * Math.sqrt(paths.length)));
    // ties (same millisecond) by depth: dependencies first when ready, users
    // first when needed
    const deeper = order === "ready" ? 1 : -1;
    const sorted = [...paths].sort((a, b) =>
      time(a) - time(b) || deeper * (depth.get(a) - depth.get(b)) || a.localeCompare(b));
    const column = new Map();
    let c = 0, members = new Set();
    for (const p of sorted) {
      if (members.size >= cap || linked(p).some((q) => members.has(q))) { c++; members = new Set(); }
      column.set(p, c);
      members.add(p);
    }
    return column;
  }

  function target_graph(box, paths, selected) {
    const columns = order === "deps" ? null : time_columns(paths);
    draw_graph(box, {
      ids: paths,
      deps: (p) => by_path.get(p).deps.map(([d, peek]) => [d, peek, by_path.get(p).changed_set.has(d)]),
      lines: (p) => node_lines(by_path.get(p)),
      status: (p) => by_path.get(p).status,
      held: (p) => held.has(p),
      title: (p) => node_title(by_path.get(p)),
      select: (p) => p == null ? hide_side() : show_target(p),
      open: (p) => { location.hash = href("t", { t: p }); },
      selected,
      key: location.hash,
      rank: columns ? (p) => columns.get(p) : (p) => depth.get(p),
    });
  }

  // the targets within `up` dependencies of `start` and `down` uses of it
  function around(start, up, down) {
    const out = new Set([start]);
    const walk = (dir, depth) => {
      let frontier = [start];
      for (let i = 0; i < depth && frontier.length; i++) {
        const next = [];
        for (const p of frontier) {
          const n = by_path.get(p);
          for (const [q] of dir === "up" ? n.deps : n.dependents)
            if (by_path.has(q) && !out.has(q)) { out.add(q); next.push(q); }
        }
        frontier = next;
      }
    };
    walk("up", up);
    walk("down", down);
    return [...out];
  }

  /////////////// live controls ///////////////

  async function act(action, args = {}) {
    try {
      const r = await fetch("api/control", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        credentials: "same-origin",
        body: JSON.stringify({ action, ...args }),
      });
      if (r.status === 401) { connection = "logged out"; show_live(); return; }
      if (!r.ok) throw new Error(`${r.status}`);
      control = await r.json();
      held = new Map(control.waiting.map((w) => [w.path, w]));
      show_live();
      refresh_side();
    } catch (e) {
      alert(`That didn't reach the run: ${e.message}`);
    }
  }

  const has_break = (at) => control.breakpoints.includes(at);

  // the header's live part: the run's state and its main controls
  function show_live() {
    const box = $("live");
    if (!LIVE) { box.hidden = true; return; }
    box.hidden = false;
    let state, cls;
    if (connection === "logged out") { state = "logged out"; cls = "bad"; }
    else if (connection !== "live") { state = connection === "connecting" ? "connecting…" : "waiting for a run…"; cls = "off"; }
    else if (G?.done) { state = "finished"; cls = "off"; }
    else if (at_end()) { state = "done, waiting to exit"; cls = "paused"; }
    else if (control.paused) { state = "paused"; cls = "paused"; }
    else { state = "running"; cls = "on"; }
    const hits = control.waiting.filter((w) => w.hit);
    box.innerHTML =
      `<span class="pill ${cls}">${state}</span>` +
      (connection === "logged out" ? ` <a href="login">log in</a>` : "") +
      (connection === "live" && !G?.done ?
        (at_end()
          ? ` <button class="btn" data-act="resume">Let it exit</button>`
          : control.paused
          ? ` <button class="btn" data-act="resume">Resume</button>`
          : ` <button class="btn" data-act="pause">Pause</button>`) : "") +
      ` <a href="#debug">${control.waiting.length} held · ${control.breakpoints.length} breakpoint${control.breakpoints.length === 1 ? "" : "s"}</a>` +
      (hits.length ? ` <span class="hit">stopped at ${target_link(hits[hits.length - 1].path, short_kind(by_path.get(hits[hits.length - 1].path)?.kind ?? hits[hits.length - 1].path))}</span>` : "") +
      ` <button class="btn quiet" data-act="logout" title="Log out">Log out</button>`;
    document.title = (control.paused && !G?.done ? "⏸ " : "") + "Run graph";
  }

  $("live").addEventListener("click", async (e) => {
    const a = e.target.closest("[data-act]")?.dataset.act;
    if (a === "logout") {
      await fetch("logout", { method: "POST", credentials: "same-origin" });
      location.href = "login";
    } else if (a) act(a);
  });

  // buttons anywhere else: release a held target, toggle a breakpoint
  document.addEventListener("click", (e) => {
    const b = e.target.closest("[data-release], [data-break]");
    if (!b || b.closest("#live")) return;
    if (b.dataset.release) act("release", { id: +b.dataset.release });
    else act("break", { at: b.dataset.break, on: b.dataset.on === "1" });
  });

  function break_button(at, text) {
    const on = has_break(at);
    return `<button class="btn" data-break="${esc(at)}" data-on="${on ? "0" : "1"}">${on ? "Remove breakpoint" : esc(text)}</button>`;
  }

  /////////////// side panel ///////////////

  const side = $("side");
  let side_shows = null;  // [kind of thing, its id], to show again on updates
  function hide_side() { side.hidden = true; side.innerHTML = ""; side_shows = null; }
  function open_side(html, keep_scroll) {
    const top = side.scrollTop;
    side.hidden = false;
    side.innerHTML = `<button class="btn close" title="Close">✕</button>` + html;
    side.querySelector(".close").onclick = hide_side;
    side.scrollTop = keep_scroll ? top : 0;
  }
  function refresh_side() {
    if (!side_shows) return;
    const [what, id] = side_shows;
    if (what === "t") show_target(id, true); else show_kind(id, true);
  }

  function list_targets(entries, n) {
    if (!entries.length) return `<p class="muted">none</p>`;
    return `<ul class="links">${entries.map(([p, peek]) => {
      const m = by_path.get(p);
      const changed = n && n.changed_set.has(p);
      return `<li class="${changed ? "changed" : ""}"><span class="dot st-${m ? m.status : "reused"}"></span>` +
        target_link(p) + (peek ? `<span class="tag">peek</span>` : "") +
        (changed ? `<span class="tag">changed</span>` : "") +
        (held.has(p) ? `<span class="tag held">held</span>` : "") + `</li>`;
    }).join("")}</ul>`;
  }

  function command_item(c) {
    const bad = c.rc == null || c.rc !== 0;
    const files = [c.out && `<a href="${esc(c.out)}" target="_blank">stdout</a>`,
      c.err && `<a href="${esc(c.err)}" target="_blank">stderr</a>`].filter(Boolean).join(" · ");
    return `<li class="${bad ? "bad" : ""}"><div class="line">$ ${esc(shell(c.command))}</div>` +
      `<div class="meta">${c.id != null ? `#${c.id} · ` : ""}in <code>${esc(c.cwd)}</code> · ` +
      `${c.rc == null ? "didn't finish" : `exit ${c.rc}`} · ${ms(c.ms)} at ${at(c.start)}` +
      `${files ? " · " + files : ""}</div></li>`;
  }

  const GATES = { Start: "before it starts", Run: "before its function runs", Failed: "after it failed", End: "before it exits" };
  const why_held = (w) => !w.hit ? "paused" : ({ Failed: "failed", End: "--pause-at-end" })[w.gate] ?? "breakpoint";
  const at_end = () => control.waiting.some((w) => w.gate === "End");

  function show_target(path, keep_scroll) {
    const n = by_path.get(path);
    if (!n) return;
    side_shows = ["t", path];
    const h = held.get(path);
    const files = [n.log && `<a href="${esc(n.log)}" target="_blank">log.txt</a>`,
      n.saved && `<a href="${esc(n.saved)}" target="_blank">saved.json</a>`,
      n.data && `<a href="${esc(n.data)}" target="_blank">data/</a>`].filter(Boolean).join(" · ");
    const controls = LIVE && n.path !== ROOT && !G.done ?
      `<p class="controls">${h ? `<button class="btn primary" data-release="${h.id}">Release</button> ` : ""}` +
      `${break_button(n.path, "Break here")} ${break_button(n.kind, `Break on every ${short_kind(n.kind)}`)}</p>` : "";
    open_side(
      `<h2>${esc(n.path)}</h2>` +
      (h ? `<p class="held-note">Held ${GATES[h.gate] ?? h.gate} (${why_held(h)}).</p>` : "") +
      `<p>${badge(n.status)} ${n.reason ? esc(n.reason) : ""}</p>` + controls +
      (n.error ? `<div class="error">${esc(n.error)}</div>` : "") +
      `<dl>` +
      (n.path !== ROOT ? `<dt>kind</dt><dd><a href="${esc(href("targets", { k: n.kind }))}">${esc(n.kind)}</a></dd>` : "") +
      (n.start != null ? `<dt>needed at</dt><dd>${at(n.start)}</dd>` : "") +
      (n.total != null ? `<dt>total</dt><dd>${ms(n.total)} <span class="muted">(with its dependencies)</span></dd>` : "") +
      (n.own != null ? `<dt>own</dt><dd>${ms(n.own)} <span class="muted">(its function)</span></dd>` : "") +
      (files ? `<dt>files</dt><dd>${files}</dd>` : "") +
      `</dl>` +
      `<p><a href="${esc(href("t", { t: n.path }))}">Show around this target</a></p>` +
      `<h3>Commands (${n.commands.length})</h3>` +
      (n.commands.length ? `<ul class="cmds">${n.commands.map(command_item).join("")}</ul>` : `<p class="muted">none</p>`) +
      (n.changed.length ? `<h3>Changed dependencies (${n.changed.length})</h3>` +
        list_targets(n.changed.map((p) => [p, false]), n) : "") +
      `<h3>Depends on (${n.deps.length})</h3>` + list_targets(n.deps, n) +
      `<h3>Used by (${n.dependents.length})</h3>` + list_targets(n.dependents, null),
      keep_scroll);
  }

  function show_kind(id, keep_scroll) {
    const k = kinds.get(id);
    if (!k) return;
    side_shows = ["k", id];
    const counts = STATUSES.filter((s) => k.counts[s]).map((s) => `${badge(s)} ${k.counts[s]}`).join(" ");
    open_side(
      `<h2>${esc(id)}</h2><p>${counts}</p>` +
      (LIVE && !G.done && id !== ROOT ? `<p class="controls">${break_button(id, `Break on every ${short_kind(id)}`)}</p>` : "") +
      `<dl><dt>targets</dt><dd>${k.nodes.length}</dd><dt>own time</dt><dd>${ms(k.own)} <span class="muted">(all of them)</span></dd></dl>` +
      `<p><a href="${esc(href("targets", { k: id }))}">List its targets</a> · ` +
      `<a href="${esc(href("graph", { k: id }))}">Show them in the graph</a></p>` +
      `<h3>Depends on</h3><ul class="links">${[...k.deps].map(([d, e]) =>
        `<li><span class="dot st-${kind_status(kinds.get(d))}"></span>${esc(short_kind(d))} <span class="tag">×${e.n}${e.peek ? " peek" : ""}</span></li>`).join("") || "<li class=muted>none</li>"}</ul>`,
      keep_scroll);
  }

  /////////////// views ///////////////

  const main = $("main");

  function legend() {
    return `<span class="legend">${["ran", "reused", "failed", "blocked", "used"].map((s) =>
      `<span><i class="st-${s}"></i>${label(s)}</span>`).join("")}` +
      (LIVE ? `<span><i class="held-key"></i>held</span>` : "") +
      `<span><svg width="26" height="8"><path class="edge changed" d="M0,4 H26"/></svg> changed</span>` +
      `<span><svg width="26" height="8"><path class="edge peek" d="M0,4 H26"/></svg> peek</span></span>`;
  }

  function view_overview() {
    main.className = "";
    const count = (s) => G.nodes.filter((n) => n.status === s).length;
    const failed = G.nodes.filter((n) => n.status === "failed");
    const slow = G.nodes.filter((n) => n.own != null).sort((a, b) => b.own - a.own).slice(0, 12);
    const slow_cmds = [...all_commands].sort((a, b) => b.ms - a.ms).slice(0, 12);
    main.innerHTML = `<div class="page">
      <div class="cards">
        <div class="card"><b>${G.nodes.length - 1}</b><span>targets used</span></div>
        <div class="card"><b>${count("ran")}</b><span>ran</span></div>
        <div class="card"><b>${count("reused")}</b><span>reused</span></div>
        <div class="card"><b>${count("failed") + count("blocked")}</b><span>failed</span></div>
        ${LIVE && !G.done ? `<div class="card"><b>${count("used")}</b><span>running</span></div>` : ""}
        <div class="card"><b>${all_commands.length}</b><span>commands</span></div>
        <div class="card"><b>${ms(G.ms)}</b><span>${G.done ? "run time" : "so far"}</span></div>
      </div>
      <h2>Definitions</h2>
      <p class="muted">One box per definition; its bar shows how its targets got their values. Click for details, double-click to list its targets. ${legend()}</p>
      <div class="canvas fixed" id="kinds"></div>
      ${failed.length ? `<h2>Failures (${failed.length})</h2><table><tbody>${failed.map((n) =>
        `<tr data-t="${esc(n.path)}"><td>${target_link(n.path)}</td><td class="error">${esc(n.error)}</td></tr>`).join("")}</tbody></table>` : ""}
      <h2>Slowest targets</h2>
      ${slow.length ? `<table><thead><tr><th>target</th><th class="num">own</th><th class="num">commands</th></tr></thead><tbody>${slow.map((n) =>
        `<tr data-t="${esc(n.path)}"><td>${target_link(n.path)}</td><td class="num">${ms(n.own)}</td><td class="num">${n.commands.length}</td></tr>`).join("")}</tbody></table>` : `<p class="muted">no target ran</p>`}
      <h2>Slowest commands</h2>
      ${commands_table(slow_cmds)}
    </div>`;
    const ids = [...kinds.keys()];
    draw_graph($("kinds"), {
      ids,
      deps: (k) => [...kinds.get(k).deps].map(([d, e]) => [d, e.peek, false]),
      lines: (k) => [short_kind(k), k === ROOT ? "" : `×${kinds.get(k).nodes.length}`],
      status: (k) => kind_status(kinds.get(k)),
      held: (k) => kinds.get(k).nodes.some((n) => held.has(n.path)),
      bar: (k) => k === ROOT ? null : kinds.get(k).counts,
      title: (k) => `${k}\n` + STATUSES.filter((s) => kinds.get(k).counts[s]).map((s) => `${label(s)}: ${kinds.get(k).counts[s]}`).join(", "),
      select: (k) => k == null ? hide_side() : k === ROOT ? show_target(ROOT) : show_kind(k),
      open: (k) => { location.hash = k === ROOT ? href("t", { t: ROOT }) : href("targets", { k }); },
      key: "#overview",
    });
  }

  function view_graph(args) {
    main.className = "full";
    const all = args.get("all") === "1";
    const k = args.get("k");
    let paths;
    if (k) {
      const set = new Set();
      for (const n of kinds.get(k)?.nodes || []) {
        set.add(n.path);
        n.deps.forEach(([d]) => set.add(d));
        n.dependents.forEach(([d]) => set.add(d));
      }
      paths = [...set];
    } else if (all) {
      paths = G.nodes.map((n) => n.path);
    } else {
      // what ran (or failed, or is running), with what it depends on
      const set = new Set([ROOT]);
      for (const n of G.nodes) {
        if (n.status === "reused") continue;
        set.add(n.path);
        n.deps.forEach(([d]) => set.add(d));
      }
      paths = [...set];
    }
    paths = paths.filter((p) => by_path.has(p));
    main.innerHTML = `<div class="toolbar">
        ${k ? `<span>${esc(short_kind(k))}'s targets, with what they use and what uses them</span> <a href="#graph">all that ran</a>` :
          `<label><input type="checkbox" id="all" ${all ? "checked" : ""}> include targets that only were reused</label>`}
        ${order_select()}
        <span class="note">${paths.length} targets · drag to pan, scroll to zoom, click for details, double-click to focus</span>
        ${legend()}
      </div><div class="canvas" id="canvas"></div>`;
    watch_order();
    $("all")?.addEventListener("change", (e) => { location.hash = e.target.checked ? "#graph&all=1" : "#graph"; });
    if (paths.length > 6000)
      $("canvas").innerHTML = `<p class="page">${paths.length} targets are too many to draw at once: pick a definition on the <a href="#overview">overview</a>.</p>`;
    else target_graph($("canvas"), paths, side_shows?.[0] === "t" ? side_shows[1] : args.get("sel"));
  }

  function view_target(args) {
    const path = args.get("t");
    const n = by_path.get(path);
    if (!n) {
      main.className = "";
      main.innerHTML = `<p class="page">No target <code>${esc(path)}</code> in this run${LIVE && !G.done ? " yet" : ""}.</p>`;
      return;
    }
    main.className = "full";
    const up = +(args.get("up") ?? 2), down = +(args.get("down") ?? 1);
    const paths = around(path, up, down);
    const step = (key, d) => href("t", { t: path, up: key === "up" ? Math.max(0, up + d) : up, down: key === "down" ? Math.max(0, down + d) : down });
    main.innerHTML = `<div class="toolbar">
        <span>${badge(n.status)} <code>${esc(path)}</code></span>
        <span>dependencies: ${up} levels <a class="btn" href="${esc(step("up", -1))}">−</a> <a class="btn" href="${esc(step("up", 1))}">+</a></span>
        <span>uses: ${down} levels <a class="btn" href="${esc(step("down", -1))}">−</a> <a class="btn" href="${esc(step("down", 1))}">+</a></span>
        ${order_select()} <span class="note">${paths.length} targets</span> ${legend()}
      </div><div class="canvas" id="canvas"></div>`;
    watch_order();
    const shown = side_shows?.[0] === "t" && by_path.has(side_shows[1]) ? side_shows[1] : path;
    target_graph($("canvas"), paths, shown);
    show_target(shown, true);
  }

  function view_targets(args) {
    main.className = "";
    const k = args.get("k") || "", q = (args.get("q") || "").toLowerCase(), o = args.get("o") || "";
    const sort = args.get("sort") || "path";
    let rows = G.nodes.filter((n) => (!k || n.kind === k) && (!o || n.status === o) && (!q || n.path.toLowerCase().includes(q)));
    const by = { path: (a, b) => a.path.localeCompare(b.path), own: (a, b) => (b.own ?? -1) - (a.own ?? -1),
      total: (a, b) => (b.total ?? -1) - (a.total ?? -1), commands: (a, b) => b.commands.length - a.commands.length,
      start: (a, b) => (a.start ?? 1e15) - (b.start ?? 1e15) };
    rows.sort(by[sort] || by.path);
    const shown = rows.slice(0, 3000);
    const kind_opts = [...kinds.keys()].sort().map((id) => `<option value="${esc(id)}" ${id === k ? "selected" : ""}>${esc(id)}</option>`).join("");
    const out_opts = ["ran", "reused", "failed", "blocked", "used"].map((s) => `<option value="${s}" ${s === o ? "selected" : ""}>${label(s)}</option>`).join("");
    const th = (key, text, cls = "") => `<th class="${cls}" data-sort="${key}">${text}${sort === key ? " ▾" : ""}</th>`;
    main.innerHTML = `<div class="toolbar">
        <select id="fk"><option value="">all definitions</option>${kind_opts}</select>
        <select id="fo"><option value="">any outcome</option>${out_opts}</select>
        <input id="fq" type="search" placeholder="path contains" value="${esc(q)}">
        <span class="note">${rows.length} targets${rows.length > shown.length ? `, showing ${shown.length}` : ""}</span>
        ${k ? `<a href="${esc(href("graph", { k }))}">show in the graph</a>` : ""}
        ${k && LIVE && !G.done ? break_button(k, `Break on every ${short_kind(k)}`) : ""}
      </div>
      <div class="page"><table><thead><tr>${th("status", "outcome")}${th("path", "target")}<th>why it ran</th>
        ${th("start", "needed at", "num")}${th("total", "total", "num")}${th("own", "own", "num")}${th("commands", "commands", "num")}</tr></thead>
      <tbody>${shown.map((n) => `<tr data-t="${esc(n.path)}"><td>${badge(n.status)}${held.has(n.path) ? ` <span class="tag held">held</span>` : ""}</td><td class="mono">${esc(n.path)}</td>
        <td>${esc(n.reason ?? "")}${n.changed.length ? ` <span class="muted">(${n.changed.length})</span>` : ""}</td>
        <td class="num">${at(n.start)}</td><td class="num">${ms(n.total)}</td><td class="num">${ms(n.own)}</td><td class="num">${n.commands.length || ""}</td></tr>`).join("")}
      </tbody></table></div>`;
    const go = (changes) => { location.hash = href("targets", { targets: "", k, q, o, sort, ...changes }); };
    $("fk").onchange = (e) => go({ k: e.target.value });
    $("fo").onchange = (e) => go({ o: e.target.value });
    $("fq").onchange = (e) => go({ q: e.target.value });
    complete_in($("fq"));
    main.querySelectorAll("th[data-sort]").forEach((el) => {
      el.onclick = () => go({ sort: el.dataset.sort === "status" ? "path" : el.dataset.sort });
    });
  }

  function commands_table(cmds) {
    if (!cmds.length) return `<p class="muted">no commands ran</p>`;
    return `<table><thead><tr><th class="num">at</th><th class="num">time</th><th class="num">exit</th><th>target</th><th>command</th><th></th></tr></thead><tbody>` +
      cmds.map((c) => `<tr data-t="${esc(c.node.path)}"><td class="num">${at(c.start)}</td><td class="num">${ms(c.ms)}</td>` +
        `<td class="num">${c.rc == null ? "—" : c.rc}</td><td class="mono">${esc(middle(c.node.path, 60))}</td>` +
        `<td class="cmd" title="${esc(c.cwd)}">${esc(middle(shell(c.command), 160))}</td>` +
        `<td>${[c.out && `<a href="${esc(c.out)}" target="_blank">out</a>`, c.err && `<a href="${esc(c.err)}" target="_blank">err</a>`].filter(Boolean).join(" ")}</td></tr>`).join("") +
      `</tbody></table>`;
  }

  function view_commands(args) {
    main.className = "";
    const sort = args.get("sort") || "time";
    const bad = args.get("bad") === "1";
    const q = (args.get("q") || "").toLowerCase();
    let cmds = all_commands.filter((c) => (!bad || c.rc !== 0) && (!q || shell(c.command).toLowerCase().includes(q)));
    cmds.sort(sort === "start" ? (a, b) => a.start - b.start : (a, b) => b.ms - a.ms);
    const shown = cmds.slice(0, 3000);
    main.innerHTML = `<div class="toolbar">
        <select id="cs"><option value="time" ${sort === "time" ? "selected" : ""}>slowest first</option><option value="start" ${sort === "start" ? "selected" : ""}>in order</option></select>
        <label><input type="checkbox" id="cb" ${bad ? "checked" : ""}> only failed</label>
        <input id="cq" type="search" placeholder="command contains" value="${esc(q)}">
        <span class="note">${cmds.length} commands${cmds.length > shown.length ? `, showing ${shown.length}` : ""}, ${ms(cmds.reduce((a, c) => a + c.ms, 0))} in all</span>
      </div><div class="page">${commands_table(shown)}</div>`;
    const go = (changes) => { location.hash = href("commands", { commands: "", sort, bad: bad ? "1" : "", q, ...changes }); };
    $("cs").onchange = (e) => go({ sort: e.target.value });
    $("cb").onchange = (e) => go({ bad: e.target.checked ? "1" : "" });
    $("cq").onchange = (e) => go({ q: e.target.value });
  }

  // live only: what's held, the breakpoints, and adding one
  function view_debug() {
    main.className = "";
    if (!LIVE) { main.innerHTML = `<p class="page">This page is a saved copy of a finished run: there's nothing to control.</p>`; return; }
    const running = connection === "live" && !G.done;
    main.innerHTML = `<div class="page">
      <h2>Run</h2>
      <p>${running ? (at_end()
        ? `Done, and waiting to exit (--pause-at-end). <button class="btn primary" data-act2="resume">Let it exit</button>`
        : control.paused
        ? `Paused: targets stop before they start or run. <button class="btn primary" data-act2="resume">Resume</button>`
        : `Running. <button class="btn" data-act2="pause">Pause</button>`) : "Not running: controls come back with the next run."}</p>
      <p><label><input type="checkbox" id="bof" ${control.break_on_failure ? "checked" : ""} ${running ? "" : "disabled"}> pause when a target fails (before what needs it hears)</label></p>
      <h2>Held (${control.waiting.length})</h2>
      ${control.waiting.length ? `<table><thead><tr><th>target</th><th>where</th><th>why</th><th></th></tr></thead><tbody>${control.waiting.map((w) =>
        `<tr data-t="${esc(w.path)}"><td class="mono">${target_link(w.path)}</td><td>${esc(GATES[w.gate] ?? w.gate)}</td>` +
        `<td>${why_held(w)}</td>` +
        `<td><button class="btn" data-release="${w.id}">Release</button></td></tr>`).join("")}</tbody></table>` +
        `<p class="muted">Release lets one target go on; it may stop again at its next gate while the run is paused. Resume lets them all go.</p>`
        : `<p class="muted">nothing is held</p>`}
      <h2>Breakpoints (${control.breakpoints.length})</h2>
      <p class="muted">A breakpoint holds a target just before its function runs (its dependencies are ready), and pauses the run. Set one on a target, or on a definition for all of its targets — including ones not in the graph yet.</p>
      ${control.breakpoints.length ? `<ul class="links">${control.breakpoints.map((b) =>
        `<li>${by_path.has(b) ? target_link(b) : esc(b)} <button class="btn quiet" data-break="${esc(b)}" data-on="0" title="Remove">✕</button></li>`).join("")}</ul>` : ""}
      <form id="addbp"><input id="bp" list="bp_list" placeholder="a target's path or a definition, e.g. ag/grader/Project/run_one" size="60" ${running ? "" : "disabled"}>
        <datalist id="bp_list">${[...kinds.keys()].filter((k) => k !== ROOT).sort().map((k) => `<option value="${esc(k)}">`).join("")}</datalist>
        <button class="btn" ${running ? "" : "disabled"}>Add</button></form>
    </div>`;
    main.querySelectorAll("[data-act2]").forEach((b) => { b.onclick = () => act(b.dataset.act2); });
    $("bof").onchange = (e) => act("break_on_failure", { on: e.target.checked });
    $("addbp").onsubmit = (e) => {
      e.preventDefault();
      const v = $("bp").value.trim();
      if (v) act("break", { at: v, on: true });
    };
  }

  /////////////// routing and rendering ///////////////

  function route() {
    if (!G) {
      main.className = "";
      main.innerHTML = `<p class="page muted">${connection === "logged out" ? `Your session ended: <a href="login">log in</a> again.` : "Waiting for the run…"}</p>`;
      return;
    }
    const args = new URLSearchParams(location.hash.slice(1));
    const view = ["t", "graph", "targets", "commands", "debug"].find((v) => args.has(v)) || "overview";
    document.querySelectorAll("nav a").forEach((a) =>
      a.classList.toggle("on", a.dataset.view === view || (view === "t" && a.dataset.view === "graph")));
    if (view !== "t" && view !== "graph" && view !== "overview") hide_side();
    const top = main.scrollTop;
    ({ overview: view_overview, graph: view_graph, t: view_target, targets: view_targets,
       commands: view_commands, debug: view_debug })[view](args);
    main.scrollTop = top;
    refresh_side();
  }

  function show_summary() {
    if (!G) return;
    $("summary").textContent = `${new Date(G.started).toLocaleString()} · ${ms(G.ms)}${G.done ? "" : " so far"} · ${G.workspace}`;
    $("paths").innerHTML = G.nodes.length <= 20000
      ? G.nodes.map((n) => `<option value="${esc(n.path)}">`).join("") : "";
  }

  // Live updates draw the view again, but not while the person is busy with
  // it (dragging, typing), and graphs at most every couple of seconds.
  let render_timer = null, last_render = 0;
  function schedule_render() {
    show_live();
    if (render_timer || !$("follow").checked) return;
    const graphy = main.classList.contains("full") || !!main.querySelector("#kinds");
    const wait = Math.max(0, last_render + (graphy ? 2000 : 800) - Date.now());
    render_timer = setTimeout(function again() {
      if (dragging || document.activeElement?.closest("main input, main select")) {
        render_timer = setTimeout(again, 500);
        return;
      }
      render_timer = null;
      last_render = Date.now();
      show_summary();
      route();
    }, wait);
  }

  function connect() {
    const source = new EventSource("events");
    source.addEventListener("snapshot", (e) => {
      const data = JSON.parse(e.data);
      const first = !G || G.started !== data.graph.started;
      if (first) views.clear();  // a new run: its own layout
      G = data.graph;
      control = data.control;
      connection = "live";
      index_data();
      show_summary();
      show_live();
      route();
    });
    source.addEventListener("update", (e) => {
      const data = JSON.parse(e.data);
      if (!G) return;
      const at_index = new Map(G.nodes.map((n, i) => [n.path, i]));
      for (const n of data.nodes) {
        const i = at_index.get(n.path);
        if (i == null) G.nodes.push(n); else G.nodes[i] = n;
      }
      G.ms = data.ms;
      G.done = data.done;
      control = data.control;
      index_data();
      schedule_render();
    });
    // The run ended, or the server went with it: the browser reconnects on
    // its own, which finds the next run, if there is one.
    source.onerror = async () => {
      if (connection === "live") connection = "waiting";
      show_live();
      if (source.readyState === EventSource.CLOSED) {
        // a refused stream isn't retried: see whether the session ended
        const r = await fetch("data.js", { credentials: "same-origin", redirect: "manual" }).catch(() => null);
        if (r && (r.type === "opaqueredirect" || r.status === 401 || r.status === 303)) {
          connection = "logged out";
          show_live();
          route();
        } else setTimeout(connect, 5000);
      }
    };
  }

  // clicking a table row opens its target
  main.addEventListener("click", (e) => {
    if (e.target.closest("a, button")) return;
    const row = e.target.closest("tr[data-t]");
    if (row) show_target(row.dataset.t);
  });
  document.addEventListener("keydown", (e) => {
    if (e.target.closest("input, select")) return;
    if (e.key === "Escape") hide_side();
    main.querySelector(".canvas")?.graph_keys?.(e);
  });
  complete_in($("q"));
  $("search").addEventListener("submit", (e) => {
    e.preventDefault();
    const q = $("q").value.trim();
    if (!q) return;
    location.hash = by_path.has(q) ? href("t", { t: q }) : href("targets", { q });
  });
  $("follow").addEventListener("change", (e) => { if (e.target.checked) schedule_render(); });
  window.addEventListener("hashchange", route);

  if (LIVE) {
    $("follow_box").hidden = false;
    document.querySelector('nav a[data-view="debug"]').hidden = false;
    show_live();
    route();
    connect();
  } else {
    G = GRAPH;
    index_data();
    show_summary();
    route();
  }
})();
