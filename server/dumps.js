/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const url      = require("url");
const zlib     = require("zlib");
const stream   = require("stream");
const db       = require("./database");
const http     = require("./http");
const util     = require("./utilities");
const config   = require("./config");
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

class KickAssLineProcessor extends stream.Writable {
	constructor() {
		super();
		this.processed = 0;
	}

	_write(line, _, done) {
		const segments = util.splitBuffer(line, 124);

		// Ignore invalid lines
		if (segments.length < 12)
			return done();

		insertTorrent(
			segments[1].toString("utf8"),
			segments[4].toString("utf8"),
			Number.parseInt(segments[5].toString("utf8"))
		).then(
			result => {
				this.processed += result.rows.length;
				done();
			},
			done
		);
	}
}

/**
 * Process a KickAss dump.
 * @param {Object} dump Row data
 * @returns {Promise}
 */
const processKickAssDump = function* (uri) {
	util.inform("dump: kat", "Processing '" + uri + "'");

	const req = url.parse(uri);

	req.headers = {
		"Accept": "application/x-gzip"
	};

	const stream = yield http.stream(req);
	const proc =
		stream.pipe(zlib.createGunzip())
		      .pipe(new util.Splitter(10))
		      .pipe(new KickAssLineProcessor());

	yield new Promise(function (accept, reject) {
		proc.on("finish", accept);
		proc.on("error", reject);
	});

	util.inform("dump: kat", "Added", proc.processed, "new torrents");
}.async;

/**
 * Scan all dumps and collect the contained torrents.
 * @returns {Promise}
 */
const scan = function* () {
	yield* config.dumps.kat.map(processKickAssDump);
}.async;

module.exports = {
	scan, insertTorrent
};
