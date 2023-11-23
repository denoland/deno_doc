const Fuse = window.Fuse;

const searchInput = document.querySelector("#searchbar");
const mainContentTags = document.getElementsByTagName("main");
const searchResultsDiv = document.querySelector("#searchResults");
const currentFile =
  document.querySelector("meta[name='doc-current-file']").attributes
    .getNamedItem("content").value;
const pathToRoot = "../".repeat(
  currentFile ? (currentFile.split("/").length + 1) : 0,
);
console.log(pathToRoot, currentFile);
searchInput.removeAttribute("style");

const SEARCH_INDEX = window.DENO_DOC_SEARCH_INDEX;

const fuse = new Fuse(SEARCH_INDEX.nodes, {
  keys: [{
    name: "name",
    weight: 2,
  }, {
    name: "nsQualifiers",
    weight: 1,
  }],
  isCaseSensitive: false,
  minMatchCharLength: 2,
  threshold: 0.4,
});

const loadedUrl = new URL(window.location.href);
const val = loadedUrl.searchParams.get("q");
if (val) {
  searchInput.value = val;
  doSearch(val);
}

window.addEventListener("load", function () {
  document.addEventListener("keydown", function (event) {
    if (event.key.toLowerCase() === "s") {
      if (event.target !== searchInput) {
        searchInput.focus();
        event.preventDefault();
      }
    }
  });

  const emptyPlaceholder = "Click or press 'S' to search...";
  searchInput.placeholder = emptyPlaceholder;

  searchInput.addEventListener("focus", function () {
    searchInput.placeholder = "Type your query here...";
  });

  searchInput.addEventListener("blur", function () {
    searchInput.placeholder = emptyPlaceholder;
  });
});

function debounce(func, delay) {
  let timerId;

  return function () {
    const context = this;
    const args = arguments;

    clearTimeout(timerId);

    timerId = setTimeout(function () {
      func.apply(context, args);
    }, delay);
  };
}

const debouncedSearch = debounce(doSearch, 250);

searchInput.addEventListener("input", (e) => {
  const val = e.target.value;
  debouncedSearch(val);
});

function doSearch(val) {
  if (!val) {
    updateCurrentLocation(val);
    showPage();
  } else {
    const results = searchInIndex(val);
    // console.log("results", results);
    updateCurrentLocation(val);
    renderResults(results);
    showSearchResults();
  }
}

function updateCurrentLocation(val) {
  const url = new URL(window.location.href);
  if (val) {
    url.searchParams.set("q", val);
  } else {
    url.searchParams.delete("q");
  }
  window.history.replaceState({}, "", url.href);
}

function showPage() {
  for (const mainTag of mainContentTags) {
    mainTag.style.display = "block";
  }
  searchResultsDiv.style.display = "none";
}

function showSearchResults() {
  for (const mainTag of mainContentTags) {
    mainTag.style.display = "none";
  }
  searchResultsDiv.style.display = "block";
}

function renderResults(results) {
  if (results.length === 0) {
    searchResultsDiv.innerHTML = `<span>No result</span>`;
    return;
  }

  let html = `<ul>`;

  for (const result of results) {
    // console.log("result", result);
    const [rustKind, title, symbol] = docNodeKindToStringVariants(result.kind);
    const label = result.nsQualifiers
      ? `${result.nsQualifiers}.${result.name}`
      : result.name;
    html += `<li>
<a href="${pathToRoot}${result.file}/~/${label}.html">
    <div>
        <div class="symbol_kind kind_${rustKind}_text kind_${rustKind}_bg" title="${title}">
            ${symbol}
        </div>
        <span>${label}</span>
    </div>
    <div>${result.location.filename}:${result.location.line}</div>
</a>
</li>`;
  }

  html += `</ul>`;
  searchResultsDiv.innerHTML = html;
}

function searchInIndex(val) {
  return fuse.search(val).map((result) => result.item);
}

function docNodeKindToStringVariants(kind) {
  switch (kind) {
    case "function":
      return ["Function", "Function", "f"];
    case "variable":
      return ["Variable", "Variable", "v"];
    case "class":
      return ["Class", "Class", "c"];
    case "enum":
      return ["Enum", "Enum", "E"];
    case "interface":
      return ["Interface", "Interface", "I"];
    case "typeAlias":
      return ["TypeAlias", "Type Alias", "T"];
    case "namespace":
      return ["Namespace", "Namespace", "N"];
    default:
      return [];
  }
}
