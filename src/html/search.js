const searchInput = document.querySelector("#searchbar");
const mainContentDiv = document.querySelector("#mainContent");
const searchResultsDiv = document.querySelector("#searchResults");

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
    mainContentDiv.style.display = "block";
    searchResultsDiv.style.display = "none";
}

function showSearchResults() {
    mainContentDiv.style.display = "none";
    searchResultsDiv.style.display = "block";
}

function renderResults(results) {
    let html = `<ul>`;

    for (const result of results) {
        console.log("result", result);
        html += `<li>name: ${result.name}, kind: ${result.kind}</li>`;
    }

    html += `</ul>`;
    searchResultsDiv.innerHTML = html;
}

function searchInIndex(val) {
    const valLower = val.toLowerCase();
    const results = SEARCH_INDEX.nodes.filter((node) => {
        return node.name.toLowerCase().includes(valLower);
    });
    return results;
}