/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const http     = require("../http");
const util     = require("../utilities");
const links    = require("../links");
const releases = require("../releases");
const config   = require("../config");

const FeedSchema = {
	rss: {
		channel: [{
			item: [{
				title: ["string"],
				link: ["string"]
			}]
		}]
	}
};

const process = function* (url) {
	util.inform("source: rarbg", "Processing '" + url + "'");

	const object = yield util.parseXML(yield http.download(url));

	if (!util.validateSchema(FeedSchema, object))
		throw new Error("Unrecognized feed schema");

	let insertedLinks = 0;

	yield* object.rss.channel.map(function* (channel) {
		yield* channel.item.map(function* (item) {
			const title = item.title.join();
			const link = item.link.join();

			const i = yield links.insert(
				title,
				link,
				releases.normalize(title),
				"rarbg.to"
			);

			insertedLinks += i;
		}.async);
	}.async);

	util.inform("source: rarbg", "Inserted", insertedLinks, "links");
}.async;

const scan = function* () {
	yield* config.sources.rarbg.map(process);
}.async;

module.exports = {
	scan
};
