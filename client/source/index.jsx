/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

const page     = require("page");
const React    = require("react");
const ReactDOM = require("react-dom");
const Index    = require("./components/index.jsx");
const Feed     = require("./components/feed.jsx");

// Styles
require("./index.scss");

// Navigation
function render(element) {
	ReactDOM.render(element, document.getElementById("canvas"));
}

page(/^\/feeds\/(\d+)$/, ctx => render(<Feed id={Number.parseInt(ctx.params[0])}/>));
page("/",                ctx => render(<Index />));

page(ctx => page.redirect("/"));

// Load event
window.addEventListener("load", function () {
	page();
});
