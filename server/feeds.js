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

function normalizeReleaseName(name) {
	const segments = name.split("-");
	if (segments.length > 1) segments.pop();

	return segments.join(" ").replace(/([^a-zA-Z0-9])/g, " ").replace(/\s\s+/g, " ").trim().toLowerCase();
}

const parseFeed = function* (contents) {
	const object = yield parseXML(contents);

	if (util.validateSchema(AtomSchema, object))
		return object.feed.entry.map(
			entry => normalizeReleaseName(entry.title.join(""))
		);
	else if (util.validateSchema(RSSSchema, object))
		return [].concat(...object.rss.channel.map(
			channel => channel.item.map(
				item => normalizeReleaseName(item.title.join(""))
			)
		));
	else
		throw new Error("Unrecognized feed schema");
}.async;

const feedsTable = new db.Table("feeds", "id", ["uri"]);
const feedContentsTable = new db.Table("feed_contents", null, ["feed", "release"]);

const attachRelease = function* (feed, release) {
	try {
		yield feedContentsTable.insert({feed, release});
	} catch (error) {
		if (error.code != 23505 || error.constraint != "feed_contents_feed_release_key")
			util.logError(err);
	}
}.async;

const processFeed = function* (feed) {
	util.inform("feed: " + feed.id, "Parsing '" + feed.uri + "'");

	try {
		const rels = yield parseFeed(yield http.download(feed.uri));
		util.inform("feed: " + feed.id, "Found " + rels.length + " names");

		rels.forEach(function* (name) {
			const rel = yield releases.insert(name);
			attachRelease(feed.id, rel.data.id);
		}.async);
	} catch (error) {
		util.logError(error);
	}
}.async;

const scan = function* () {
	const rows = yield feedsTable.load();
	yield* rows.map(row => processFeed(row.data));
}.async;

module.exports = {
	scan
};
