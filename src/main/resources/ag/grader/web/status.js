// The status pages' script, inlined by ag.grader.Status: click a column's
// header to sort a table.sortable by it (again to reverse).
(() => {
  const key = (td) => {
    const t = td.textContent.trim();
    const n = parseFloat(t.replace(/[%,]/g, ""));
    return /^-?[\d.,]+%?(\/\d+)?$/.test(t) && !isNaN(n) ? n : t.toLowerCase();
  };
  document.querySelectorAll("table.sortable").forEach((table) => {
    table.tHead.querySelectorAll("th").forEach((th, i) => {
      th.addEventListener("click", () => {
        const dir = th.dataset.dir === "1" ? -1 : 1;
        table.tHead.querySelectorAll("th").forEach((x) => delete x.dataset.dir);
        th.dataset.dir = dir === 1 ? "1" : "-1";
        const body = table.tBodies[0];
        const rows = [...body.rows];
        rows.sort((a, b) => {
          const x = a.cells[i] ? key(a.cells[i]) : "", y = b.cells[i] ? key(b.cells[i]) : "";
          return (typeof x === typeof y ? (x < y ? -1 : x > y ? 1 : 0) : typeof x === "number" ? -1 : 1) * dir;
        });
        rows.forEach((r) => body.append(r));
      });
    });
  });
})();
