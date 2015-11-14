"use strict";

const db    = require("./database");
const util  = require("./utilities");
const http  = require("./http");
const feeds = require("./sources/feeds");

(function* () {
	console.log(yield feeds.parseFeed(yield http.download("https://kat.cr/movies/?rss=1")));
}).async().catch(error => util.error("main", error));
