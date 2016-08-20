"use strict";

const http = require("./server/http");
const util = require("./server/utilities");

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

const parseFeed = function* (contents) {
	const object = yield util.parseXML(contents);

	let title = null, names = [];

	if (util.validateSchema(AtomSchema, object)) {
		title = object.feed.title.join("");
		names = object.feed.entry.map(
			entry => entry.title.join("")
		);
	} else if (util.validateSchema(RSSSchema, object)) {
		object.rss.channel.map(
			channel => {
				title = channel.title.join("");
				channel.item.forEach(
					item => names.push(item.title.join(""))
				);
			}
		);
	} else {
		throw new Error("Unrecognized feed schema");
	}

	return {title, names};
}.async;

const test = function* (url) {
	const contents = yield http.download(url);
	const results = [];

	for (let i = 0; i < 1000; i++)
		results.push(yield parseFeed(contents));

	return results;
}.async;

test("http://www.movie-blog.org/feed/").then(console.log);
