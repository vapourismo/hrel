/* Copyright (C) 2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const urlParser    = require("url");
const htmlEntities = require("html-entities").AllHtmlEntities;
const http         = require("../http");
const util         = require("../utilities");
const links        = require("../links");
const releases     = require("../releases");
const config       = require("../config");

const allowedHosts = config.sources.movieblog.allowedHosts;

function isAllowedHost(url) {
	const options = urlParser.parse(url);

	if (!options || (options.protocol != "https:" && options.protocol != "http:"))
		return false;

	return allowedHosts.indexOf(options.host) >= 0;
}

function mapLinks(contents, callback) {
	const linkPattern = /<a\s+target="\w+"\s+href="([^"]+)"\s*>([^<]+)<\/a>/g;
	const result = [];

	let match;
	while (match = linkPattern.exec(contents))
		result.push(
			callback(
				htmlEntities.decode(match[1]).trim(),
				htmlEntities.decode(match[2]).trim()
			)
		);

	return result;
}

const FeedSchema = {
	rss: {
		channel: [{
			item: [{
				title: ["string"],
				"content:encoded": ["string"]
			}]
		}]
	}
};

const scan = function* () {
	const url = config.sources.movieblog.url;
	util.inform("source: movieblog", "Processing '" + url + "'");

	const object = yield util.parseXML(yield http.download(url));

	if (!util.validateSchema(FeedSchema, object))
		throw new Error("Unrecognized feed schema");

	let insertedLinks = 0;

	yield* object.rss.channel.map(function* (channel) {
		yield* channel.item.map(function* (item) {
			const title = item.title.join();

			yield* mapLinks(
				item["content:encoded"].join(""),
				function* (url, hoster) {
					if (!isAllowedHost(url))
						return;

					const i = yield links.insert(
						title,
						url,
						releases.normalize(title),
						"movie-blog.org / " + hoster
					);

					insertedLinks += i;
				}.async
			);
		}.async)
	}.async);

	util.inform("source: movieblog", "Inserted", insertedLinks, "links");
}.async;

module.exports = {
	scan
};
