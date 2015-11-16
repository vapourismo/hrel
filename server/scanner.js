/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const db    = require("./database");
const util  = require("./utilities");
const feeds = require("./feeds");
const dumps = require("./dumps");

const scan = function* () {
	yield feeds.scan();
	yield dumps.scan();

	db.end();
}.async;

scan().catch(error => {
	util.logError(error);
	process.exit(1);
});
