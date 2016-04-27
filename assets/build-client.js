"use strict";

const child   = require("child_process");
const webpack = require("webpack");
// const sass    = require("node-sass");
const util    = require("../server/utilities");

webpack(require("../webpack.config.js"), function (error, stats) {
	if (error)
		return util.logError(error);

	util.inform("build-client", "Webpack compiled resources");
});

// The following is a way to compile our SCSS. Sadly the library uses deprecated features and
// therefore produces to much garbage output on the terminal for my taste.
//
// sass.render({
// 	file: "./client/source/index.scss",
// 	outFile: "./client/static/index.css"
// }, function (error, result) {
// 	if (error)
// 		return util.logError(error);

// 	util.inform("build-client", "Sass compiled resources");
// });

const sassProc = child.exec("sass client/source/index.scss:client/static/index.css");

sassProc.on("exit", function (code, signal) {
	if (code == 0)
		util.inform("build-client", "Sass compiled resources");
	else
		util.error("build-client", "Sass exited with status", code);
});
