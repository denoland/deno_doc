const searchInput = document.querySelector("#searchbar");
const mainContentTags = document.getElementsByTagName("main");
const searchResultsDiv = document.querySelector("#searchResults");

searchInput.removeAttribute("style");

const SEARCH_INDEX = window.DENO_DOC_SEARCH_INDEX;

searchInput.addEventListener("input", (e) => {
    const val = e.target.value;
    console.log("Search event.target.value", val);

    if (!val) {
        showPage();
    } else {
        const results = searchInIndex(val);
        console.log("results", results);
        renderResults(results);
        showSearchResults();
    }
});

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
    let html = `<ul>`;

    for (const result of results) {
        console.log("result", result);
        const [rustKind, title, symbol] = docNodeKindToStringVariants(result.kind);
        const label = result.nsQualifiers
            ? `${result.nsQualifiers.join(".")}.${result.name}`
            : result.name;
        html += `<li><a href="${result.name.split(".").join("/")}.html"><div class="symbol_kind kind_${rustKind}_text kind_${rustKind}_bg" title="${title}">${symbol}</div><span>${label}</span></a></li>`;
    }

    html += `</ul>`;
    searchResultsDiv.innerHTML = html;
}

function searchInIndex(val) {
    const valLower = val.toLowerCase();
    const results = SEARCH_INDEX.nodes.filter((node) => {
        const matches = node.name.toLowerCase().includes(valLower);
        
        if (matches) {
            return matches;
        }
        
        if (node.nsQualifiers) {
            return node.nsQualifiers.some((nsName) => nsName.toLowerCase().includes(valLower))
        }

        return false;
    });
    return results;
}

function docNodeKindToStringVariants(kind) {
  switch (kind) {
    case "function":
      return ["Function", "Function", "f"]
    case "variable":
      return ["Variable", "Variable", "v"]
    case "class":
      return ["Class", "Class", "c"]
    case "enum":
      return ["Enum", "Enum", "E"]
    case "interface":
      return ["Interface", "Interface", "I"]
    case "typeAlias":
      return ["TypeAlias", "Type Alias", "T"]
    case "namespace":
      return ["Namespace", "Namespace", "N"]
  }
}
