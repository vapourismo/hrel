"use strict";

const db   = require("../server/database");
const util = require("../server/utilities");

const run = function* () {
	yield db.query("DELETE FROM links");
	yield db.query("DELETE FROM feed_contents");
	yield db.query("DELETE FROM releases");
	yield db.query("DELETE FROM feeds");

	db.end();
}.async;

run().catch(error => {
	util.logError(error);
	process.exit(1);
});
