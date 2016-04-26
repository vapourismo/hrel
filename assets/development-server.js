"use strict";

const fs    = require("fs");
const path  = require("path");
const child = require("child_process");

const serverModule = path.join(path.dirname(module.filename), "..", "server", "server.js");

function forkServerModule() {
	return child.fork(serverModule, [], {
		execArgv: ["--harmony"]
	});
}

let serverInstance = forkServerModule();

fs.watch("server", {persistent: true, recursive: true}, function (event, path) {
	if (event != "change")
		return;

	console.log("Terminating server module ...");
	serverInstance.kill();

	console.log("Respawning module ...");
	serverInstance = forkServerModule();
});
