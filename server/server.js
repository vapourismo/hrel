"use strict";

const db    = require("./database");
const util  = require("./utilities");
const http  = require("./http");
const feeds = require("./feeds");
const dumps = require("./dumps");

const scan = function* () {
	yield feeds.scan();
	yield dumps.scan();
}.async;

scan().catch(util.logError);
