"use strict";

const xml2js   = require("xml2js");
const db       = require("./database");
const http     = require("./http");
const util     = require("./utilities");
const releases = require("./releases");

function parseXML(xml) {
	return new Promise(function (accept, reject) {
		xml2js.parseString(xml, function (err, result) {
			if (err) reject(err);
			else     accept(result);
		});
	});
}

const AtomSchema = {
	feed: {
		entry: [{
			title: ["string"]
		}]
	}
};

const RSSSchema = {
	rss: {
		channel: [{
			item: [{
				title: ["string"]
			}]
		}]
	}
};

const parseFeed = function* (contents) {
	const object = yield parseXML(contents);

	if (util.validateSchema(AtomSchema, object))
		return object.feed.entry.map(
			entry => releases.normalize(entry.title.join(""))
		);
	else if (util.validateSchema(RSSSchema, object))
		return [].concat(...object.rss.channel.map(
			channel => channel.item.map(
				item => releases.normalize(item.title.join(""))
			)
		));
	else
		throw new Error("Unrecognized feed schema");
}.async;

const feedsTable = new db.Table("feeds", "id", ["uri"]);
const feedContentsTable = new db.Table("feed_contents", null, ["feed", "release"]);

const attachRelease = function* (feed, release) {
	const rows = yield feedContentsTable.upsert({feed, release});
	return rows.length;
}.async;

const processFeed = function* (feed) {
	util.inform("feed: " + feed.id, "Processing '" + feed.uri + "'");

	const rels = yield parseFeed(yield http.download(feed.uri));
	let insertedReleases = 0;

	yield* rels.map(function* (name) {
		const rel = yield releases.insert(name);
		const num = yield attachRelease(feed.id, rel.data.id);
		insertedReleases += num;
	}.async);

	util.inform("feed: " + feed.id, "Found " + insertedReleases + " new releases");
}.async;

const scan = function* () {
	const rows = yield feedsTable.load();
	yield* rows.map(row => processFeed(row.data));
}.async;

module.exports = {
	scan
};
