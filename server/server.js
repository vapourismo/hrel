/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const http       = require("http");
const express    = require("express");
const bodyParser = require("body-parser");
const config     = require("./config");
const util       = require("./utilities");
const feeds      = require("./feeds");
const dumps      = require("./dumps");

var app = express();

app.use(bodyParser.json());

app.get(/^\/api\/feeds\/(\d+)$/, function (req, res) {
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

app.get("/api/feeds", function (req, res) {
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

app.post("/api/feeds", function (req, res) {
	if (!(req.body instanceof Object) || !req.body.uri)
		return res.status(400).json({error: "Invalid request body"});

	feeds.add(req.body.uri).then(
		result => {
			res.json(result);
		},
		error => {
			util.logError(error);
			res.status(400).json({error: error.message});
		}
	);
});

const sendFileOptions = {
	root: process.cwd()
};

app.get("/", function (req, res) {
	res.sendFile("./client/out/index.html", sendFileOptions);
});

app.get("/index.js", function (req, res) {
	res.sendFile("./client/out/index.js", sendFileOptions);
});

app.get("/index.css", function (req, res) {
	res.sendFile("./client/out/index.css", sendFileOptions);
});

const server = http.createServer(app);

server.on("error", function (error) {
	switch (error.code) {
		case "EADDRINUSE":
			server.close();
			util.warn("server", "Address currently in use, retrying in 1s");
			setTimeout(() => server.listen(config.server), 1000);
			break;

		default:
			util.logError(error);
			break;
	}
});

server.listen(config.server);
