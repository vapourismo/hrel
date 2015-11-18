/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const url      = require("url");
const zlib     = require("zlib");
const Lazy     = require("lazy");
const db       = require("./database");
const http     = require("./http");
const util     = require("./utilities");
const releases = require("./releases");

/**
 * Insert torrent and match it with a release.
 */
const insertTorrent = function (title, uri, size) {
	return db.query(
		"INSERT INTO torrents (title, uri, release, size) SELECT $1 :: varchar, $2 :: varchar, r.id, $3 :: bigint FROM releases r WHERE r.name = $4 :: varchar AND NOT EXISTS (SELECT * FROM torrents WHERE uri = $2 :: varchar) RETURNING id",
		[title, uri, size, releases.normalize(title)]
	);
};

/**
 * Process a KickAss dump.
 * @param {Object} dump Row data
 * @returns {Promise}
 */
const processKickAssDump = function* (dump) {
	util.inform("dump: " + dump.id, "Processing '" + dump.uri + "'");

	const req = url.parse(dump.uri);

	req.headers = {
		"Accept": "application/x-gzip"
	};

	const stream = yield http.stream(req);
	const input = stream.pipe(zlib.createGunzip());
	const proc = new Lazy(input);

	yield new Promise(function (accept, reject) {
		let insertedTorrents = 0;

		proc.lines.map(function* (line) {
			const segments = line.toString("utf8").split("|");
			if (segments.length < 12) return;

			const result = yield insertTorrent(
				segments[1], // Title
				segments[4], // URI
				segments[5]  // Size
			);

			insertedTorrents += result.rows.length;
		}.async).join(function* (ps) {
			try {
				yield* ps;
				accept();
			} catch (error) {
				reject(error);
			}

			util.inform("dump: " + dump.id, "Found " + insertedTorrents + " new torrents");
		}.async);
	});
}.async;

/**
 * Process a dump.
 * @param {Object} dump Row data
 * @returns {Promise}
 */
const processDump = function* (dump) {
	switch (dump.type) {
		case "kat":
			yield processKickAssDump(dump);
			break;

		default:
			util.error("dump: " + dump.id, "Unknown type '" + dump.type + "'");
			break;
	}
}.async;

/**
 * Scan all dumps and collect the contained torrents.
 * @returns {Promise}
 */
const scan = function* () {
	const rows = yield db.findAll("dumps");
	yield* rows.map(processDump);
}.async;

module.exports = {
	scan
};
