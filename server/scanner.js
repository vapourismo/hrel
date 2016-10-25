/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const db           = require("./database");
const util         = require("./utilities");
const feeds        = require("./feeds");
const kat          = require("./sources/kat");
const movieblog    = require("./sources/movieblog");
const thepiratebay = require("./sources/thepiratebay");
const rarbg        = require("./sources/rarbg");

const scan = function* () {
	try {
		yield feeds.scan();
	} catch (error) {
		util.error("scanner", "Error during feed process");
		util.logError(error);
	}

	try {
		yield kat.scan();
	} catch (error) {
		util.error("scanner", "Error during 'kat' process");
		util.logError(error);
	}

	try {
		yield movieblog.scan();
	} catch (error) {
		util.error("scanner", "Error during 'movieblog' process");
		util.logError(error);
	}

	try {
		yield thepiratebay.scan();
	} catch (error) {
		util.error("scanner", "Error during 'thepiratebay' process");
		util.logError(error);
	}

	try {
		yield rarbg.scan();
	} catch (error) {
		util.error("scanner", "Error during 'rarbg' process");
		util.logError(error);
	}

	db.end();
}.async;

scan().catch(error => {
	util.logError(error);
	process.exit(1);
});
