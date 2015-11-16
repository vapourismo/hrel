/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const Express = require("express");
const process = require("process");
const util    = require("./utilities");
const feeds   = require("./feeds");
const dumps   = require("./dumps");

var server = Express();

server.get("/feeds", function (req, res) {
	feeds.all().then(
		result => {
			res.json(result);
		},
		error => {
			res.status(500).end();
			util.logError(error);
		}
	);
});

server.listen(3102, "127.0.0.1");

const scan = function* () {
	yield feeds.scan();
	yield dumps.scan();
}.async;

scan().catch(error => {
	util.logError(error);
	process.exit(1);
});
