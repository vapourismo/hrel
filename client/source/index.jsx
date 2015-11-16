const page     = require("page");
const React    = require("react");
const ReactDOM = require("react-dom");
const Index    = require("./components/index.jsx");

// Styles
require("./index.scss");

// Navigation
function render(element) {
	ReactDOM.render(element, document.getElementById("canvas"));
}

page("/", ctx => render(<Index />));

page(ctx => page.redirect("/"));

// Load event
window.addEventListener("load", function () {
	page({click: false});
});
