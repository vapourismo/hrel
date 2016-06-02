/* Copyright (C) 2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const url      = require("url");
const zlib     = require("zlib");
const stream   = require("stream");
// const db       = require("../database");
const http     = require("../http");
const util     = require("../utilities");
const config   = require("../config");
const links    = require("../links");
const releases = require("../releases");

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

		const title = segments[1].toString("utf8");
		const uri = segments[4].toString("utf8");

		links.insert(
			title,
			uri,
			releases.normalize(title),
			"kat.cr"
		).then(
			insertedRows => {
				this.processed += insertedRows;
				done();
			},
			done
		);
	}
}

const process = function* (uri) {
	util.inform("source: kat", "Processing '" + uri + "'");

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

	util.inform("source: kat", "Inserted", proc.processed, "links");
}.async;

const scan = function* () {
	yield* config.sources.kat.map(process);
}.async;

module.exports = {
	scan
};
