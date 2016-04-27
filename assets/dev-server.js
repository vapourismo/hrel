"use strict";

const fs      = require("fs");
const path    = require("path");
const child   = require("child_process");
const webpack = require("webpack");
const util    = require("../server/utilities");

const serverModule = path.join(path.dirname(module.filename), "..", "server", "server.js");

function forkServerModule() {
	util.inform("dev-server", "Spawning server module");
	return child.fork(serverModule, [], {execArgv: ["--harmony"]});
}

let serverInstance = forkServerModule();

fs.watch("server", {persistent: true, recursive: true}, function (event, path) {
	if (event != "change")
		return;

	util.inform("dev-server", "Terminating server module");
	serverInstance.kill();

	serverInstance = forkServerModule();
});

webpack(require("../webpack.config.js")).watch({poll: false}, function (error, stats) {
	if (error)
		return util.logError(error);

	util.inform("dev-server", "Webpack compiled resources");
});

function compileSCSS() {
	const sassProc = child.exec("sass client/source/index.scss:client/static/index.css");

	sassProc.on("exit", function (code, signal) {
		if (code == 0)
			util.inform("dev-server", "Sass compiled resources");
		else
			util.error("dev-server", "Sass exited with status", code);
	});
}

compileSCSS();

fs.watch("client/source/index.scss", {}, function (event, path) {
	if (event != "change")
		return;

	compileSCSS();
});
