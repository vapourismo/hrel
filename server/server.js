/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const Express    = require("express");
const process    = require("process");
const bodyParser = require("body-parser");
const util       = require("./utilities");
const feeds      = require("./feeds");
const dumps      = require("./dumps");

var server = Express();

server.use(bodyParser.json());

server.get(/^\/feeds\/(\d+)$/, function (req, res) {
	feeds.one(Number.parseInt(req.params[0])).then(
		result => {
			res.json(result);
		},
		error => {
			util.logError(error);
			res.status(500).end();
		}
	);
});

server.get("/feeds", function (req, res) {
	feeds.all().then(
		result => {
			res.json(result);
		},
		error => {
			util.logError(error);
			res.status(500).end();
		}
	);
});

server.post("/feeds", function (req, res) {
	if (!(req.body instanceof Object) || !req.body.uri)
		return res.status(400).json({error: "Invalid request body"});

	feeds.add(req.body.uri).then(
		result => {
			res.json(result);
		},
		error => {
			res.status(400).json({error: error.message});
		}
	);
});

server.listen(3102, "127.0.0.1");
