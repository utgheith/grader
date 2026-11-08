// The run graph's viewer, made into one page with graph.html and graph.css
// by ag.r2.RunGraph. GRAPH, from data.js, is what the run did: every target
// it used, how each got its value, what each depends on, and the commands
// that ran to compute it.
(() => {
  "use strict";

  /////////////// the data ///////////////

  const G = GRAPH;
  const ROOT = G.root;
  const by_path = new Map();
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
    n.cmd_ms = n.commands.reduce((a, c) => a + c.ms, 0);
  }

  // a definition's short name: its last two parts, e.g. Project/run_one
  const short_kind = (k) => k === ROOT ? ROOT : k.split("/").slice(-2).join("/");
  // what tells a target apart from the kind's other targets
  const params = (n) => n.path === n.kind ? "" :
    n.path.startsWith(n.kind + "/") ? n.path.slice(n.kind.length + 1) : n.path;

  const kinds = new Map();
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
  const STATUSES = ["ran", "failed", "blocked", "used", "reused", "root"];
  const LABEL = {
    ran: "ran", reused: "reused", failed: "failed", blocked: "failed (dependency)",
    used: "never finished", root: "the run",
  };
  // a kind's color: the most telling of its targets' outcomes
  const kind_status = (k) =>
    ["failed", "blocked", "ran", "used", "root"].find((s) => k.counts[s]) || "reused";

  const all_commands = G.nodes.flatMap((n) => n.commands.map((c) => ({ ...c, node: n })));

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
  const badge = (status) => `<span class="badge st-${status}">${esc(LABEL[status])}</span>`;
  const $ = (id) => document.getElementById(id);

  /////////////// layout ///////////////

  // Layered layout, dependencies to the left of what uses them. `ids` are the
  // nodes to show, deps(id) the ids it depends on (others are ignored), and
  // size(id) its [width, height]. Tall columns wrap, at more rows for bigger
  // graphs, to keep them from getting much wider than they're tall.
  const ROW_GAP = 10, COL_GAP = 50, LAYER_GAP = 120;
  function layout(ids, deps, size) {
    const MAX_ROWS = Math.max(40, Math.ceil(3 * Math.sqrt(ids.length)));
    const shown = new Set(ids);
    const preds = new Map(), succs = new Map();
    for (const id of ids) { preds.set(id, []); succs.set(id, []); }
    for (const id of ids)
      for (const d of deps(id))
        if (shown.has(d) && d !== id) { preds.get(id).push(d); succs.get(d).push(id); }

    // layer: the longest chain of dependencies below it
    const layer = new Map(), indeg = new Map();
    for (const id of ids) { indeg.set(id, preds.get(id).length); layer.set(id, 0); }
    const queue = ids.filter((id) => indeg.get(id) === 0);
    for (let i = 0; i < queue.length; i++) {
      const id = queue[i];
      for (const s of succs.get(id)) {
        layer.set(s, Math.max(layer.get(s), layer.get(id) + 1));
        indeg.set(s, indeg.get(s) - 1);
        if (indeg.get(s) === 0) queue.push(s);
      }
    }
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

  // Draws a graph in `box`, which can be panned (drag), zoomed (wheel, the
  // buttons) and clicked. spec: ids, deps(id) -> [[dep, peek, changed]],
  // lines(id) -> [title, subtitle], status(id), bar(id) -> counts or null,
  // title(id), select(id), open(id), selected.
  function draw_graph(box, spec) {
    const CH = 6.6, PAD = 12;
    const lines = new Map(spec.ids.map((id) => [id, spec.lines(id)]));
    const sizes = new Map(spec.ids.map((id) => {
      const [a, b] = lines.get(id);
      const w = Math.min(330, Math.max(110, Math.max(a.length * 7, b.length * CH) + 2 * PAD));
      return [id, [w, b ? 38 : 26]];
    }));
    const edges_of = new Map(spec.ids.map((id) => [id, spec.deps(id)]));
    const L = layout(spec.ids, (id) => edges_of.get(id).map((e) => e[0]), (id) => sizes.get(id));

    const index = new Map(spec.ids.map((id, i) => [id, i]));
    const parts = [];
    const edge_list = [];
    for (const id of spec.ids) {
      const b = L.at.get(id);
      for (const [d, peek, changed] of edges_of.get(id)) {
        const a = L.at.get(d);
        if (!a || d === id) continue;
        const x1 = a.x + a.w, y1 = a.y + a.h / 2, x2 = b.x, y2 = b.y + b.h / 2;
        const dx = Math.max(40, (x2 - x1) / 2);
        const cls = "edge" + (peek ? " peek" : "") + (changed ? " changed" : "");
        parts.push(`<path class="${cls}" data-e="${edge_list.length}" d="M${x1},${y1} C${x1 + dx},${y1} ${x2 - dx},${y2} ${x2},${y2}"/>`);
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
      parts.push(
        `<g class="node st-${spec.status(id)}" data-i="${i}" transform="translate(${p.x},${p.y})">` +
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
    const apply = () => vp.setAttribute("transform", `translate(${view.x},${view.y}) scale(${view.k})`);
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
      svg.classList.remove("dragging");
      if (moved) return;
      const el = document.elementFromPoint(e.clientX, e.clientY)?.closest(".node");
      if (el) select(+el.dataset.i, true);
      else select(-1, true);
    };
    svg.addEventListener("pointerup", end_drag);
    svg.addEventListener("pointercancel", () => { drag = null; svg.classList.remove("dragging"); });
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

    requestAnimationFrame(() => {
      fit();
      const i = spec.selected != null ? index.get(spec.selected) : undefined;
      if (i != null) {
        select(i, false);
        if (spec.ids.length > 30) center_on(i);
      }
    });
    box.graph_keys = (e) => { if (e.key === "f") fit(); };
    return { count: spec.ids.length };
  }

  /////////////// the target graphs ///////////////

  const node_lines = (n) => n.path === ROOT ? [ROOT, ""] : [short_kind(n.kind), params(n)];
  const node_title = (n) => `${n.path}\n${LABEL[n.status]}${n.reason ? ": " + n.reason : ""}` +
    (n.own != null ? `\nown time ${ms(n.own)}` : "") +
    (n.commands.length ? `\n${n.commands.length} command(s)` : "");

  function target_graph(box, paths, selected) {
    return draw_graph(box, {
      ids: paths,
      deps: (p) => by_path.get(p).deps.map(([d, peek]) => [d, peek, by_path.get(p).changed_set.has(d)]),
      lines: (p) => node_lines(by_path.get(p)),
      status: (p) => by_path.get(p).status,
      title: (p) => node_title(by_path.get(p)),
      select: (p) => p == null ? hide_side() : show_target(p),
      open: (p) => { location.hash = href("t", { t: p }); },
      selected,
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

  /////////////// side panel ///////////////

  const side = $("side");
  function hide_side() { side.hidden = true; side.innerHTML = ""; }
  function open_side(html) {
    side.hidden = false;
    side.innerHTML = `<button class="btn close" title="Close">✕</button>` + html;
    side.querySelector(".close").onclick = hide_side;
    side.scrollTop = 0;
  }

  function list_targets(entries, n) {
    if (!entries.length) return `<p class="muted">none</p>`;
    return `<ul class="links">${entries.map(([p, peek]) => {
      const m = by_path.get(p);
      const changed = n && n.changed_set.has(p);
      return `<li class="${changed ? "changed" : ""}"><span class="dot st-${m ? m.status : "reused"}"></span>` +
        target_link(p) + (peek ? `<span class="tag">peek</span>` : "") +
        (changed ? `<span class="tag">changed</span>` : "") + `</li>`;
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

  function show_target(path) {
    const n = by_path.get(path);
    if (!n) return;
    const files = [n.log && `<a href="${esc(n.log)}" target="_blank">log.txt</a>`,
      n.saved && `<a href="${esc(n.saved)}" target="_blank">saved.json</a>`,
      n.data && `<a href="${esc(n.data)}" target="_blank">data/</a>`].filter(Boolean).join(" · ");
    open_side(
      `<h2>${esc(n.path)}</h2>` +
      `<p>${badge(n.status)} ${n.reason ? esc(n.reason) : ""}</p>` +
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
      `<h3>Used by (${n.dependents.length})</h3>` + list_targets(n.dependents, null));
  }

  function show_kind(id) {
    const k = kinds.get(id);
    if (!k) return;
    const counts = STATUSES.filter((s) => k.counts[s]).map((s) => `${badge(s)} ${k.counts[s]}`).join(" ");
    open_side(
      `<h2>${esc(id)}</h2><p>${counts}</p>` +
      `<dl><dt>targets</dt><dd>${k.nodes.length}</dd><dt>own time</dt><dd>${ms(k.own)} <span class="muted">(all of them)</span></dd></dl>` +
      `<p><a href="${esc(href("targets", { k: id }))}">List its targets</a> · ` +
      `<a href="${esc(href("graph", { k: id }))}">Show them in the graph</a></p>` +
      `<h3>Depends on</h3><ul class="links">${[...k.deps].map(([d, e]) =>
        `<li><span class="dot st-${kind_status(kinds.get(d))}"></span>${esc(short_kind(d))} <span class="tag">×${e.n}${e.peek ? " peek" : ""}</span></li>`).join("") || "<li class=muted>none</li>"}</ul>`);
  }

  /////////////// views ///////////////

  const main = $("main");

  function legend() {
    return `<span class="legend">${["ran", "reused", "failed", "blocked", "used"].map((s) =>
      `<span><i class="st-${s}"></i>${LABEL[s]}</span>`).join("")}` +
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
        <div class="card st-ran"><b>${count("ran")}</b><span>ran</span></div>
        <div class="card"><b>${count("reused")}</b><span>reused</span></div>
        <div class="card"><b>${count("failed") + count("blocked")}</b><span>failed</span></div>
        <div class="card"><b>${all_commands.length}</b><span>commands</span></div>
        <div class="card"><b>${ms(G.ms)}</b><span>run time</span></div>
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
      bar: (k) => k === ROOT ? null : kinds.get(k).counts,
      title: (k) => `${k}\n` + STATUSES.filter((s) => kinds.get(k).counts[s]).map((s) => `${LABEL[s]}: ${kinds.get(k).counts[s]}`).join(", "),
      select: (k) => k == null ? hide_side() : k === ROOT ? show_target(ROOT) : show_kind(k),
      open: (k) => { location.hash = k === ROOT ? href("t", { t: ROOT }) : href("targets", { k }); },
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
      // what ran (or failed), with what it depends on
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
        <span class="note">${paths.length} targets · drag to pan, scroll to zoom, click for details, double-click to focus</span>
        ${legend()}
      </div><div class="canvas" id="canvas"></div>`;
    $("all")?.addEventListener("change", (e) => { location.hash = e.target.checked ? "#graph&all=1" : "#graph"; });
    if (paths.length > 6000)
      $("canvas").innerHTML = `<p class="page">${paths.length} targets are too many to draw at once: pick a definition on the <a href="#overview">overview</a>.</p>`;
    else target_graph($("canvas"), paths, args.get("sel"));
  }

  function view_target(args) {
    const path = args.get("t");
    const n = by_path.get(path);
    if (!n) { main.className = ""; main.innerHTML = `<p class="page">No target <code>${esc(path)}</code> in this run.</p>`; return; }
    main.className = "full";
    const up = +(args.get("up") ?? 2), down = +(args.get("down") ?? 1);
    const paths = around(path, up, down);
    const step = (key, d) => href("t", { t: path, up: key === "up" ? Math.max(0, up + d) : up, down: key === "down" ? Math.max(0, down + d) : down });
    main.innerHTML = `<div class="toolbar">
        <span>${badge(n.status)} <code>${esc(path)}</code></span>
        <span>dependencies: ${up} levels <a class="btn" href="${esc(step("up", -1))}">−</a> <a class="btn" href="${esc(step("up", 1))}">+</a></span>
        <span>uses: ${down} levels <a class="btn" href="${esc(step("down", -1))}">−</a> <a class="btn" href="${esc(step("down", 1))}">+</a></span>
        <span class="note">${paths.length} targets</span> ${legend()}
      </div><div class="canvas" id="canvas"></div>`;
    target_graph($("canvas"), paths, path);
    show_target(path);
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
    const out_opts = ["ran", "reused", "failed", "blocked", "used"].map((s) => `<option value="${s}" ${s === o ? "selected" : ""}>${LABEL[s]}</option>`).join("");
    const th = (key, label, cls = "") => `<th class="${cls}" data-sort="${key}">${label}${sort === key ? " ▾" : ""}</th>`;
    main.innerHTML = `<div class="toolbar">
        <select id="fk"><option value="">all definitions</option>${kind_opts}</select>
        <select id="fo"><option value="">any outcome</option>${out_opts}</select>
        <input id="fq" type="search" placeholder="path contains" value="${esc(q)}">
        <span class="note">${rows.length} targets${rows.length > shown.length ? `, showing ${shown.length}` : ""}</span>
        ${k ? `<a href="${esc(href("graph", { k }))}">show in the graph</a>` : ""}
      </div>
      <div class="page"><table><thead><tr>${th("status", "outcome")}${th("path", "target")}<th>why it ran</th>
        ${th("start", "needed at", "num")}${th("total", "total", "num")}${th("own", "own", "num")}${th("commands", "commands", "num")}</tr></thead>
      <tbody>${shown.map((n) => `<tr data-t="${esc(n.path)}"><td>${badge(n.status)}</td><td class="mono">${esc(n.path)}</td>
        <td>${esc(n.reason ?? "")}${n.changed.length ? ` <span class="muted">(${n.changed.length})</span>` : ""}</td>
        <td class="num">${at(n.start)}</td><td class="num">${ms(n.total)}</td><td class="num">${ms(n.own)}</td><td class="num">${n.commands.length || ""}</td></tr>`).join("")}
      </tbody></table></div>`;
    const go = (changes) => {
      const a = { targets: "", k, q, o, sort, ...changes };
      location.hash = href("targets", a);
    };
    $("fk").onchange = (e) => go({ k: e.target.value });
    $("fo").onchange = (e) => go({ o: e.target.value });
    $("fq").onchange = (e) => go({ q: e.target.value });
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

  /////////////// routing ///////////////

  function route() {
    const args = new URLSearchParams(location.hash.slice(1));
    const view = ["t", "graph", "targets", "commands"].find((v) => args.has(v)) || "overview";
    document.querySelectorAll("nav a").forEach((a) =>
      a.classList.toggle("on", a.dataset.view === view || (view === "t" && a.dataset.view === "graph")));
    if (view !== "t") hide_side();
    ({ overview: view_overview, graph: view_graph, t: view_target, targets: view_targets, commands: view_commands })[view](args);
  }

  // clicking a table row opens its target
  main.addEventListener("click", (e) => {
    if (e.target.closest("a")) return;
    const row = e.target.closest("tr[data-t]");
    if (row) show_target(row.dataset.t);
  });
  document.addEventListener("keydown", (e) => {
    if (e.target.closest("input, select")) return;
    if (e.key === "Escape") hide_side();
    main.querySelector(".canvas")?.graph_keys?.(e);
  });

  $("paths").innerHTML = G.nodes.length <= 20000
    ? G.nodes.map((n) => `<option value="${esc(n.path)}">`).join("") : "";
  $("search").addEventListener("submit", (e) => {
    e.preventDefault();
    const q = $("q").value.trim();
    if (!q) return;
    location.hash = by_path.has(q) ? href("t", { t: q }) : href("targets", { q });
  });

  const started = new Date(G.started);
  $("summary").textContent = `${started.toLocaleString()} · ${ms(G.ms)} · ${G.workspace}`;
  window.addEventListener("hashchange", route);
  route();
})();
