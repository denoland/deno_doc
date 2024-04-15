function findParent(el, find) {
  do {
    if (find(el)) {
      return el;
    }
  } while (el = el.parentElement);
}

document.addEventListener("click", (e) => {
  const target = findParent(
    e.target,
    (el) => el instanceof HTMLButtonElement && el.dataset["copy"],
  );
  if (target) {
    navigator?.clipboard?.writeText(target.dataset["copy"]);
  }
});

window.addEventListener("load", () => {
  const checkbox = document.getElementById("usageDropdownInput");
  document.addEventListener("mouseup", (e) => {
    const label = findParent(
      e.target,
      (el) =>
        el instanceof HTMLLabelElement && el.htmlFor === "usageDropdownInput",
    );
    if (!label) {
      checkbox.checked = false;
    }
  });
});
