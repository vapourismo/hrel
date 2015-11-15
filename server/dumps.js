"use strict";

const url      = require("url");
const zlib     = require("zlib");
const db       = require("./database");
const http     = require("./http");
const util     = require("./utilities");
const releases = require("./releases");

const table = new db.Table("dumps", "id", ["uri", "type"]);

/**
 * Decompress gzipped data.
 * @param {Buffer} contents GZipped data
 * @returns {Buffer} Gunzipped data
 */
const decompressGZip = function (contents) {
	return new Promise(function (accept, reject) {
		zlib.gunzip(contents, function (error, result) {
			if (error) reject(error);
			else       accept(result);
		});
	});
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

	const buf = yield decompressGZip(yield http.download(req));
	const contents = buf.toString("utf8");
	let insertedTorrents = 0;

	yield* contents.split("\n").map(function* (line) {
		const segments = line.split("|");
		if (segments.length < 12) return;

		const result = yield db.query(
			"INSERT INTO torrents (title, uri, release, size) SELECT $1 :: varchar, $2 :: varchar, r.id, $3 :: bigint FROM releases r WHERE r.name = $4 :: varchar AND NOT EXISTS (SELECT * FROM torrents WHERE uri = $2 :: varchar) RETURNING id",
			[segments[1], segments[4], segments[5], releases.normalize(segments[1])]
		);

		insertedTorrents += result.rows.length;
	}.async);

	util.inform("dump: " + dump.id, "Found " + insertedTorrents + " new torrents");
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
	const rows = yield table.load();
	yield* rows.map(row => processDump(row.data));
}.async;

module.exports = {
	scan
};
