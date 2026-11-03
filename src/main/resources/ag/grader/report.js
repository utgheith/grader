// The report page's script, inlined into each page by ag.grader.HtmlGen,
// which fills in the count of fixed (non-test) columns at the start of
// every row, below.
(() => {
  const FIXED = FIXED_COLUMNS;
  // Settings are per project (switching tabs loads the other project's).
  // Same key as earlier versions of this page, so saved pins carry over.
  let key, config, sort;
  function load_config() {
    key = `~gheith/${document.body.dataset.assignment}.html#config`;
    config = {};
    try { config = JSON.parse(localStorage.getItem(key) || "{}") || {}; } catch (e) {}
    config.pinned_tests = config.pinned_tests || {};
    config.pinned_aliases = config.pinned_aliases || {};
    sort = config.sort || { key: "alias", dir: 1 };
  }
  load_config();
  const save = () => { try { localStorage.setItem(key, JSON.stringify(config)); } catch (e) {} };

  // Wires up the current <main>. Runs again whenever a newer version of
  // the page is swapped in.
  function setup() {
    const table = document.querySelector("table.results");
    const thead = table.tHead;
    const tbody = table.tBodies[0];
    const head = thead.rows[0];
    const test_headers = Array.from(head.cells).slice(FIXED);
    const col_of = new Map(test_headers.map((th) => [th.dataset.id, th.dataset.col]));
    const student_rows = () => Array.from(table.rows).filter((r) => r.dataset.alias);

    // Columns: pinned tests first, each group in the page's default order.
    function order_columns() {
      const pinned = (th) => !!config.pinned_tests[th.dataset.id];
      const rank = new Map(test_headers
        .slice()
        .sort((a, b) => (+pinned(b) - +pinned(a)) || (a.dataset.col - b.dataset.col))
        .map((th, i) => [th.dataset.col, i]));
      for (const row of table.rows) {
        const cells = Array.from(row.cells);
        const tests = cells.slice(FIXED).sort((a, b) => rank.get(a.dataset.col) - rank.get(b.dataset.col));
        row.replaceChildren(...cells.slice(0, FIXED), ...tests);
      }
      for (const th of test_headers) {
        th.classList.toggle("pinned", pinned(th));
        th.querySelector("input.colpin").checked = pinned(th);
      }
    }

    // Rows: pinned rows live in <thead> (so they stay visible while
    // scrolling); both groups are sorted the same way.
    function value_of(row) {
      if (sort.key === "alias") return row.dataset.alias;
      if (sort.key === "chosen" || sort.key === "all" || sort.key === "score") return +row.dataset[sort.key];
      const col = col_of.get(sort.key.slice("test:".length));
      const cell = col === undefined ? null : row.querySelector(`td[data-col="${col}"]`);
      return cell ? +cell.dataset.r : -1;
    }
    function sort_rows() {
      for (const section of [thead, tbody]) {
        const rows = Array.from(section.rows)
          .filter((r) => r.dataset.alias)
          .map((r) => [r, value_of(r)]);
        rows.sort(([ra, a], [rb, b]) => {
          const c = typeof a === "string" ? a.localeCompare(b) : a - b;
          return (c * sort.dir) || ra.dataset.alias.localeCompare(rb.dataset.alias);
        });
        section.append(...rows.map(([r]) => r));
      }
      for (const button of head.querySelectorAll("button.sort")) {
        const th = button.closest("th");
        if (button.dataset.key === sort.key) th.setAttribute("aria-sort", sort.dir > 0 ? "ascending" : "descending");
        else th.removeAttribute("aria-sort");
      }
    }

    function pin_row(row, pinned) {
      (pinned ? thead : tbody).append(row);
      row.querySelector("input.rowpin").checked = pinned;
    }

    head.addEventListener("click", (e) => {
      const button = e.target.closest("button.sort");
      if (!button) return;
      const k = button.dataset.key;
      // numbers and test results sort best-first; aliases ascending
      sort = sort.key === k ? { key: k, dir: -sort.dir } : { key: k, dir: k === "alias" ? 1 : -1 };
      config.sort = sort;
      save();
      sort_rows();
    });

    table.addEventListener("change", (e) => {
      const box = e.target;
      if (box.matches("input.rowpin")) {
        const row = box.closest("tr");
        config.pinned_aliases[row.dataset.alias] = box.checked;
        pin_row(row, box.checked);
        sort_rows();
      } else if (box.matches("input.colpin")) {
        config.pinned_tests[box.dataset.id] = box.checked;
        order_columns();
      } else return;
      save();
    });

    table.addEventListener("click", (e) => {
      const cell = e.target.closest("td.alias");
      if (!cell) return;
      cell.parentElement.classList.toggle("marked");
      show_compare(cell.parentElement.dataset.alias);
    });

    document.getElementById("compare-pick").addEventListener("change", (e) => show_compare(e.target.value));
    const notable = document.getElementById("notable-only");
    notable.checked = !!config.notable_only;
    notable.addEventListener("change", () => {
      config.notable_only = notable.checked;
      save();
      show_compare(compare, false);
    });

    const filter = document.getElementById("filter");
    filter.addEventListener("input", () => {
      const q = filter.value.trim();
      for (const row of student_rows()) row.hidden = q !== "" && !row.dataset.alias.includes(q);
    });

    const only = document.getElementById("only-chosen");
    only.checked = !!config.only_chosen;
    const apply_only = () => table.classList.toggle("only-chosen", only.checked);
    only.addEventListener("change", () => {
      config.only_chosen = only.checked;
      save();
      apply_only();
    });

    document.getElementById("clear-pins").addEventListener("click", () => {
      config.pinned_tests = {};
      config.pinned_aliases = {};
      save();
      for (const row of student_rows()) pin_row(row, false);
      order_columns();
      sort_rows();
    });

    for (const row of Array.from(tbody.rows)) {
      if (config.pinned_aliases[row.dataset.alias]) pin_row(row, true);
    }
    order_columns();
    sort_rows();
    apply_only();
    show_compare(compare, false);
  }

  // ---- Compare one submission with all the others, from the data the
  // page embeds (#report-data).
  const hash_compare = () => decodeURIComponent((location.hash.match(/compare=([^&]+)/) || [])[1] || "");
  let compare = hash_compare() || config.compare || "";

  function el(tag, props, ...kids) {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(props || {})) {
      if (k === "class") e.className = v; else e.setAttribute(k, v);
    }
    e.append(...kids.filter((k) => k !== null && k !== undefined));
    return e;
  }
  const pct = (x) => `${Math.round(100 * x)}%`;
  const secs = (t) => (t === null || t === undefined) ? "\u2013" : `${t < 10 ? t.toFixed(2) : t.toFixed(1)}s`;
  const median = (xs) => {
    if (!xs.length) return null;
    const s = xs.slice().sort((a, b) => a - b);
    const m = s.length >> 1;
    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
  };
  const GLYPH = { 3: ["pass", "\u00b7"], 2: ["timeout", "T"], 1: ["fail", "\u2717"], 0: ["unknown", "?"] };

  // Where `mine` stands among `all` (higher is better): rank 1 = best.
  function standing(mine, all) {
    const better = all.filter((x) => x > mine).length;
    const worse = all.filter((x) => x < mine).length;
    return { rank: better + 1, beats: all.length > 1 ? worse / (all.length - 1) : 0 };
  }

  function histogram(values, mine, max) {
    const ns = "http://www.w3.org/2000/svg";
    const counts = Array(max + 1).fill(0);
    for (const v of values) counts[v]++;
    const top = Math.max(1, ...counts);
    const svg = document.createElementNS(ns, "svg");
    svg.setAttribute("class", "hist");
    svg.setAttribute("viewBox", `0 0 ${(max + 1) * 10} 50`);
    svg.setAttribute("preserveAspectRatio", "none");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", `distribution of tests passed; this submission passed ${mine}`);
    counts.forEach((c, v) => {
      if (!c) return;
      const h = Math.max(2, 50 * c / top);
      const r = document.createElementNS(ns, "rect");
      r.setAttribute("x", v * 10 + 1);
      r.setAttribute("width", 8);
      r.setAttribute("y", 50 - h);
      r.setAttribute("height", h);
      if (v === mine) r.setAttribute("class", "me");
      const t = document.createElementNS(ns, "title");
      t.textContent = `${c} submission${c === 1 ? "" : "s"} passed ${v}`;
      r.append(t);
      svg.append(r);
    });
    return svg;
  }

  function show_compare(alias, remember = true) {
    const body = document.getElementById("compare-body");
    const pick = document.getElementById("compare-pick");
    const data = JSON.parse(document.getElementById("report-data").textContent);
    compare = data.rows[alias] ? alias : "";
    pick.value = compare;
    if (remember) {
      config.compare = compare;
      save();
      history.replaceState(null, "", compare ? `#compare=${encodeURIComponent(compare)}` : location.pathname + location.search);
    }
    body.replaceChildren();
    if (!compare) {
      body.append(el("p", { class: "note" }, "Pick a submission above, or click an alias in the grid, to see how it compares with all the others."));
      return;
    }

    const aliases = Object.keys(data.rows);
    const mine = data.rows[compare];
    const n_chosen = data.chosen.filter((c) => c).length;
    const passed = (a, only_chosen) =>
      data.rows[a].filter((c, i) => c[0] === 3 && (!only_chosen || data.chosen[i])).length;

    // correctness
    const summary = el("div", { class: "summary" });
    const score_card = (label, only_chosen, out_of) => {
      const all = aliases.map((a) => passed(a, only_chosen));
      const me = passed(compare, only_chosen);
      const s = standing(me, all);
      summary.append(el("div", { class: "stat" },
        el("div", { class: "stat-value" }, `${me}`, el("small", {}, ` / ${out_of} ${label}`)),
        el("div", { class: "stat-label" },
          `rank ${s.rank} of ${all.length} \u00b7 passes more than ${pct(s.beats)} of submissions`),
        histogram(all, me, out_of),
        el("div", { class: "hist-axis" }, el("span", {}, "0"), el("span", {}, `${out_of} passed`))));
    };
    const my_score = data.scores[compare];
    if (my_score !== null && my_score !== undefined) {
      const all = aliases.map((a) => data.scores[a]).filter((x) => x !== null);
      const s = standing(my_score, all);
      summary.append(el("div", { class: "stat" },
        el("div", { class: "stat-value" }, my_score.toFixed(3), el("small", {}, " correctness"),
          el("span", { class: "exp" }, "experimental")),
        el("div", { class: "stat-label" },
          `rank ${s.rank} of ${all.length} \u00b7 higher than ${pct(s.beats)} of submissions`)));
    }
    if (n_chosen > 0) score_card("chosen tests", true, n_chosen);
    score_card("tests", false, data.tests.length);

    // performance, per test and overall
    const rows = data.tests.map((t, i) => {
      const [r, p, n, time] = mine[i];
      const rate = aliases.filter((a) => data.rows[a][i][0] === 3).length / aliases.length;
      const times = aliases.map((a) => data.rows[a][i][3]).filter((x) => x !== null);
      const med = median(times);
      const ratio = (time !== null && med) ? time / med : null;
      const flags = [];
      const what = { 2: "times out", 1: "fails", 0: "didn't run" }[r];
      if (r !== 3 && rate >= 0.75) flags.push(["bad", `${what}, but ${pct(rate)} pass`]);
      if (r === 3 && rate <= 0.25) flags.push(["good", `passes; only ${pct(rate)} do`]);
      if (p > 0 && p < n) flags.push(["warn", `flaky: ${p}/${n} tries passed`]);
      if (ratio !== null && ratio >= 2) flags.push(["warn", "much slower than typical"]);
      return { t, i, r, p, n, time, rate, med, ratio, flags };
    });
    const ratios = rows.filter((x) => x.ratio !== null && x.ratio > 0).map((x) => x.ratio);
    const typical = ratios.length ? Math.exp(ratios.reduce((s, x) => s + Math.log(x), 0) / ratios.length) : null;
    summary.append(el("div", { class: "stat" },
      el("div", { class: "stat-value" }, typical === null ? "\u2013" : `${typical.toFixed(2)}\u00d7`,
        el("small", {}, " median time")),
      el("div", { class: "stat-label" }, typical === null
        ? "no timing data for the tests it passes"
        : `typical speed over the ${ratios.length} tests it passes; ` +
          `faster than the median on ${ratios.filter((x) => x < 1).length}`)));
    body.append(summary);

    const shown = document.getElementById("notable-only").checked ? rows.filter((x) => x.flags.length) : rows;
    if (!shown.length) {
      body.append(el("p", { class: "note" }, "Nothing stands out: this submission does about as well as the others on every test."));
      return;
    }
    const rel = (x) => {
      if (x.ratio === null) return el("span", { class: "rel" }, "\u2013");
      if (x.ratio >= 1.1) return el("span", { class: "rel slow" }, `${x.ratio.toFixed(1)}\u00d7 slower`);
      if (x.ratio <= 0.9) return el("span", { class: "rel fast" }, `${(1 / x.ratio).toFixed(1)}\u00d7 faster`);
      return el("span", { class: "rel" }, "about the median");
    };
    const table = el("table", { class: "cmp" },
      el("thead", {}, el("tr", {},
        el("th", {}, "test"), el("th", {}, "this submission"), el("th", {}, "submissions passing"),
        el("th", { class: "num" }, "time"), el("th", { class: "num" }, "median"), el("th", {}, "relative"),
        el("th", {}, ""))),
      el("tbody", {}, ...shown.map((x) => {
        const [cls, glyph] = GLYPH[x.r];
        return el("tr", { class: data.chosen[x.i] ? "is-chosen" : "" },
          el("td", {}, x.t),
          el("td", {}, el("span", { class: `cell ${cls}` }, glyph), x.n ? `${x.p}/${x.n} tries` : "not run"),
          el("td", {}, el("span", { class: "meter" }, el("span", { style: `width: ${pct(x.rate)}` })), pct(x.rate)),
          el("td", { class: "num" }, secs(x.time)),
          el("td", { class: "num" }, secs(x.med)),
          el("td", {}, rel(x)),
          el("td", {}, ...x.flags.map(([k, f]) => el("span", { class: `flag ${k}` }, f))));
      })));
    body.append(el("div", { class: "cmp-wrap" }, table));
  }

  setup();

  // ---- Live updates: poll for a newer version of this page and swap it
  // in place, keeping the viewer's scroll position, filter and highlights.
  const POLL_MS = 60 * 1000;
  const version_of = (doc) => doc.querySelector('meta[name="report-version"]')?.content;

  // "alias|test" -> result rank, for flashing the cells that changed
  function results_of(root) {
    const id_of = new Map(Array.from(root.querySelectorAll("th.test")).map((th) => [th.dataset.col, th.dataset.id]));
    const out = new Map();
    for (const td of root.querySelectorAll("td.cell")) {
      out.set(`${td.parentElement.dataset.alias}|${id_of.get(td.dataset.col)}`, td.dataset.r);
    }
    return [out, id_of];
  }

  let toast_timer;
  function toast(message) {
    let t = document.querySelector(".toast");
    if (!t) {
      t = document.createElement("div");
      t.className = "toast";
      t.setAttribute("role", "status");
      document.body.append(t);
    }
    t.textContent = message;
    t.classList.add("show");
    clearTimeout(toast_timer);
    toast_timer = setTimeout(() => t.classList.remove("show"), 5000);
  }

  function swap_in(doc) {
    const old_main = document.querySelector("main");
    const new_main = doc.querySelector("main");
    const new_header = doc.querySelector("header.top");
    if (!new_main || !new_header) return;

    const wrap = old_main.querySelector(".table-wrap");
    const scroll = [wrap.scrollTop, wrap.scrollLeft];
    const filter = document.getElementById("filter").value;
    const marked = new Set(Array.from(old_main.querySelectorAll("tr.marked")).map((r) => r.dataset.alias));
    const [before] = results_of(old_main);

    document.querySelector('meta[name="report-version"]').content = version_of(doc);
    document.querySelector("header.top").replaceWith(document.adoptNode(new_header));
    old_main.replaceWith(document.adoptNode(new_main));
    setup();

    const main = document.querySelector("main");
    const box = document.getElementById("filter");
    box.value = filter;
    box.dispatchEvent(new Event("input"));
    for (const r of main.querySelectorAll("tr[data-alias]")) {
      if (marked.has(r.dataset.alias)) r.classList.add("marked");
    }
    const new_wrap = main.querySelector(".table-wrap");
    new_wrap.scrollTop = scroll[0];
    new_wrap.scrollLeft = scroll[1];

    const [after, id_of] = results_of(main);
    let changed = 0;
    for (const td of main.querySelectorAll("td.cell")) {
      const k = `${td.parentElement.dataset.alias}|${id_of.get(td.dataset.col)}`;
      if (before.has(k) && before.get(k) !== after.get(k)) {
        td.classList.add("changed");
        changed++;
      }
    }
    const time = new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
    toast(`Updated at ${time}` + (changed ? ` — ${changed} result${changed === 1 ? "" : "s"} changed` : ""));
  }

  let checking = false;
  async function check() {
    // a page opened from disk can't fetch itself
    if (checking || location.protocol === "file:") return;
    checking = true;
    try {
      // "no-cache" revalidates with the server, which just says "not
      // modified" while the page is unchanged
      const res = await fetch(location.href, { cache: "no-cache" });
      if (!res.ok) return;
      const doc = new DOMParser().parseFromString(await res.text(), "text/html");
      const v = version_of(doc);
      // (a tab switch may have happened while this was in flight)
      const same = doc.body && doc.body.dataset.assignment === document.body.dataset.assignment &&
        doc.body.dataset.phase === document.body.dataset.phase;
      if (same && v && v !== version_of(document)) swap_in(doc);
    } catch (e) {
      // offline or briefly unavailable: try again next time
    } finally {
      checking = false;
    }
  }

  let timer;
  function schedule() {
    clearTimeout(timer);
    if (!document.hidden) timer = setTimeout(async () => { await check(); schedule(); }, POLL_MS);
  }
  // stop polling in background tabs; check as soon as one comes back
  document.addEventListener("visibilitychange", () => {
    if (!document.hidden) check();
    schedule();
  });
  schedule();

  // ---- Project tabs: switch to a sibling project's page in place, like
  // following its link but without reloading.
  let loaded_path = location.pathname;
  async function open_project(href, push) {
    let doc;
    try {
      const res = await fetch(href, { cache: "no-cache" });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      doc = new DOMParser().parseFromString(await res.text(), "text/html");
    } catch (e) {
      location.href = href; // fall back to a plain page load
      return;
    }
    const main = doc.querySelector("main");
    const header = doc.querySelector("header.top");
    if (!main || !header || !doc.getElementById("report-data")) {
      location.href = href;
      return;
    }
    if (push) history.pushState(null, "", href);
    loaded_path = location.pathname;
    document.title = doc.title;
    document.body.dataset.assignment = doc.body.dataset.assignment;
    document.body.dataset.phase = doc.body.dataset.phase;
    document.querySelector('meta[name="report-version"]').content = version_of(doc);
    document.querySelector("header.top").replaceWith(document.adoptNode(header));
    document.querySelector("main").replaceWith(document.adoptNode(main));
    load_config();
    compare = hash_compare() || config.compare || "";
    setup();
  }

  document.addEventListener("click", (e) => {
    const link = e.target.closest("nav.tabs a, nav.phases a");
    // let new-tab / new-window clicks, and pages opened from disk, behave normally
    if (!link || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    if (location.protocol === "file:") return;
    e.preventDefault();
    if (!link.classList.contains("active")) open_project(link.href, true);
  });
  // Back / Forward between projects; a hash-only change just re-compares
  window.addEventListener("popstate", () => {
    if (location.pathname !== loaded_path) open_project(location.href, false);
    else show_compare(hash_compare() || compare, false);
  });
})();
