/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

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

/**
 * Insert a new release.
 * @param {String} name Normalized release name
 */
const insert = function* (name) {
	const row = yield db.insertUnique("releases", {name}, null, true);
	return row || (yield db.findExactly("releases", {name})).pop();
}.async;

module.exports = {
	normalize,
	insert
};
