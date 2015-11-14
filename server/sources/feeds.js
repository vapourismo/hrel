"use strict";

const xml2js = require("xml2js");
const util   = require("../utilities");

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

module.exports = {
	normalizeReleaseName,
	parseFeed
};
