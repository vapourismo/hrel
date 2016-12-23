/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

import React from "react";
import ReactDOM from "react-dom";

// Root
class Root extends React.Component {
	render() {
		return null;
	}
}

// Load event
window.addEventListener("load", function () {
	ReactDOM.render(<Root />, document.getElementById("canvas"));
});
