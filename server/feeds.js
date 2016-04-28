/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const xml2js   = require("xml2js");
const validURL = require("valid-url");
const db       = require("./database");
const http     = require("./http");
const util     = require("./utilities");
const releases = require("./releases");

/**
 * Summon Satan,
 * @param {Buffer} xml XML data
 * @returns {Promise<Object>}
 */
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
		title: ["string"],
		entry: [{
			title: ["string"]
		}]
	}
};

const RSSSchema = {
	rss: {
		channel: [{
			title: ["string"],
			item: [{
				title: ["string"]
			}]
		}]
	}
};

/**
 * Parse a RSS or Atom feed in order to retrieve release names from it.
 * @param {Buffer} contents Raw feed contents
 * @returns {Promise<Array<String>>} List of release names
 */
const parseFeed = function* (contents) {
	const object = yield parseXML(contents);

	let title = null, names = [];

	if (util.validateSchema(AtomSchema, object)) {
		title = object.feed.title.join("");
		names = object.feed.entry.map(
			entry => releases.normalize(entry.title.join(""))
		);
	} else if (util.validateSchema(RSSSchema, object)) {
		object.rss.channel.map(
			channel => {
				title = channel.title.join("");
				channel.item.forEach(
					item => names.push(releases.normalize(item.title.join("")))
				);
			}
		);
	} else {
		throw new Error("Unrecognized feed schema");
	}

	return {title, names};
}.async;

/**
 * Connect a release to a feed.
 * @param {Number} feed    Feed ID
 * @param {Number} release Release ID
 * @returns {Promise<Number>} Number of newly connected releases
 */
const attachRelease = function* (feed, release) {
	const row = yield db.insertUnique("feed_contents", {feed, release}, null, true);
	return row ? 1 : 0;
}.async;

/**
 * Process a feed.
 * @param {Object} feed Row data
 * @return {Promise}
 */
const processFeed = function* (feed) {
	util.inform("feed: " + feed.id, "Processing '" + feed.uri + "'");

	const result = yield parseFeed(yield http.download(feed.uri));

	if (result.title)
		yield db.updateExactly("feeds", {title: result.title}, {id: feed.id});

	let insertedReleases = 0;
	yield* result.names.map(function* (name) {
		const rel = yield releases.insert(name);
		insertedReleases += yield attachRelease(feed.id, rel.id);
	}.async);

	util.inform("feed: " + feed.id, "Found " + insertedReleases + " new releases");
}.async;

/**
 * Scan all feeds and collect the contained release names.
 */
const scan = function* () {
	const rows = yield db.findAll("feeds");
	yield* rows.map(function* (feed) {
		try {
			yield processFeed(feed);
		} catch (error) {
			util.error("feed: " + feed.id, "Failed to process");
		}
	}.async);
}.async;

/**
 * Retrieve all feeds.
 */
const all = function* () {
	const result = yield db.query("SELECT f.id, f.uri, f.title, count_feed_torrents(f.id) AS ntorrents, count_feed_releases(f.id) as nreleases FROM feeds f ORDER BY f.title ASC");
	return result.rows;
}.async;

/**count
 * Retrieve information for a feed.
 */
const one = function* (fid) {
	const result = yield db.query("SELECT t.id, t.title, t.uri, t.size, t.inserted FROM feed_contents fc, releases r, torrents t WHERE fc.feed = $1 AND fc.release = r.id AND r.id = t.release ORDER BY t.inserted DESC LIMIT 100", [fid]);
	return {releases: result.rows};
}.async;

/**
 * Add a new feed.
 */
const add = function* (uri) {
	if (!validURL.isHttpUri(uri) && !validURL.isHttpsUri(uri))
		throw new Error("Invalid URI");

	// Check if the feed already exists
	let feed = (yield db.findExactly("feeds", {uri})).pop();
	if (feed) return {id: feed.id};

	// Download feeds up to 10M
	const contents = yield http.download(uri, 10485760);
	let result;

	// Parse the feed, catch exceptions to notify the user of invalid feeds
	try {
		result = yield parseFeed(contents);
	} catch (error) {
		throw new Error("Given URI does not point to a valid RSS or Atom feed");
	}

	// Insert the new release
	feed = yield db.insert("feeds", result.title ? {uri, title: result.title} : {uri}, true);

	let insertedReleases = 0;
	yield* result.names.map(function* (name) {
		const rel = yield releases.insert(name);
		insertedReleases += yield attachRelease(feed.id, rel.id);
	}.async);

	util.inform("feed: " + feed.id, "Found " + insertedReleases + " new releases");

	return {id: feed.id};
}.async;

module.exports = {
	scan,
	all,
	one,
	add
};
