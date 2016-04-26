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

const tvEpisodePattern = /^([a-zA-Z0-9 ]+)\s+[sS](\d+)[eE](\d+)/;
const moviePattern = /^([a-zA-Z0-9 ]+)\s+(\d{4,})/;

/**
 * Analyse a normalized release name.
 * @param {String} name Normalized release name
 * @return {Object} Release information
 */
function analyze(name) {
	let result = tvEpisodePattern.exec(name);
	if (result)
		return {
			type:    "tvepisode",
			name:    result[1],
			season:  Number.parseInt(result[2]),
			episode: Number.parseInt(result[3]),
			tags:    name.substring(result[0].length).trim().split(" ")
		};

	result = moviePattern.exec(name);
	if (result)
		return {
			type: "movie",
			name: result[1],
			year: Number.parseInt(result[2]),
			tags: name.substring(result[0].length).trim().split(" ")
		};

	return null;
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
	analyze,
	insert
};
