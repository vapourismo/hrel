const React    = require("react");
const ReactDOM = require("react-dom");

// Styles
require("./index.scss");

// Load event
window.addEventListener("load", function () {
	ReactDOM.render(
		<div>Hello World</div>,
		document.getElementById("canvas")
	);
});
