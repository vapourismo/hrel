/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const url      = require("url");
const zlib     = require("zlib");
const stream   = require("stream");
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

class KickAssLineProcessor extends stream.Writable {
	constructor() {
		super();

		this.stash = [];
	}

	_write(line, _, done) {
		const segments = util.splitBuffer(line, 124);

		// Ignore invalid lines
		if (segments.length < 12)
			return done();

		const promise = insertTorrent(
			segments[1].toString("utf8"),                 // Title
			segments[4].toString("utf8"),                 // URI
			Number.parseInt(segments[5].toString("utf8")) // Size
		);

		this.stash.push(promise);

		if (this.stash.length >= 10000)
			this.processStash().then(done, done);
		else
			done();
	}
}

KickAssLineProcessor.prototype.processStash = function* (accept, reject) {
	yield* this.stash;
	this.stash = [];
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
	const proc =
		stream.pipe(zlib.createGunzip())
		      .pipe(new util.Splitter(10))
		      .pipe(new KickAssLineProcessor());

	yield new Promise(function (accept, reject) {
		proc.on("finish", () => proc.processStash().then(accept, reject));
		proc.on("error", reject);
	});

	util.debug("dump: " + dump.id, "Done processing");
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
	scan, insertTorrent
};
