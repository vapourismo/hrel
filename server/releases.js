"use strict";

const db = require("./database");

/**
 * Normalize a release name so that it is properly comparable.
 * @param {String} name Release name
 * @returns {String} Normalized release name
 */
function normalize(name) {
	const segments = name.split("-");
	if (segments.length > 1) segments.pop();

	return segments.join(" ").replace(/([^a-zA-Z0-9])/g, " ").replace(/\s\s+/g, " ").trim().toLowerCase();
}

const table = new db.Table("releases", "id", ["name"]);

/**
 * Insert a new release.
 * @param {String} name Normalized release name
 */
const insert = function* (name) {
	const rows = yield table.upsert({name});

	if (rows.length > 0)
		return rows.pop();
	else
		return (yield table.find({name})).pop();
}.async;

module.exports = {
	normalize,
	insert
};
