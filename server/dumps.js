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
 * Parse a line from a KickAss dump.
 * @param {Buffer} line Input line
 * @return {Promise<Number>} Number of inserted torrents
 */
const processKickAssDumpLine = function* (line) {
	const segments = util.splitBuffer(line, 124);

	// Ignore invalid lines
	if (segments.length < 12)
		return 0;

	const result = yield insertTorrent(
		segments[1].toString("utf8"),                 // Title
		segments[4].toString("utf8"),                 // URI
		Number.parseInt(segments[5].toString("utf8")) // Size
	);

	return result.rows.length;
}.async;

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
	const proc = new Lazy(stream.pipe(zlib.createGunzip()));

	yield new Promise(function (accept, reject) {
		proc.lines.map(processKickAssDumpLine).join(function* (ps) {
			try {
				let insertedTorrents = 0;
				for (let i = 0; i < ps.length; i++)
					insertedTorrents += yield ps[i];

				accept();
				util.inform("dump: " + dump.id, "Found " + insertedTorrents + " new torrents");
			} catch (error) {
				reject(error);
			}
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
