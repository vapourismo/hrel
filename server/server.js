/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const Express = require("express");
const util    = require("./utilities");
const feeds   = require("./feeds");

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
