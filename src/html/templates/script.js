document.addEventListener("click", (e) => {
  if (e.target instanceof HTMLButtonElement && e.target.dataset["copy"]) {
    return navigator?.clipboard?.writeText(e.target.dataset["copy"]);
  }
});
