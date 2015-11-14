"use strict";

const http  = require("http");
const https = require("https");
const zlib  = require("zlib");
const util  = require("./utilities");
const feeds = require("./sources/feeds");

function download(url) {
	return new Promise(function (accept, reject) {
		const schema = /^https:\/\//.test(url) ? https : http;

		const req = schema.request(url, function (res) {
			const chunks = [];

			res.on("data", function (chunk) {
				chunks.push(chunk);
			})

			res.on("end", function () {
				const data = Buffer.concat(chunks);

				switch (res.headers["content-encoding"]) {
					case "gzip":
						zlib.gunzip(data, function (error, result) {
							if (error) reject(error);
							else       accept(result);
						});
						break;

					case "deflate":
						zlib.inflate(data, function (error, result) {
							if (error) reject(error);
							else       accept(result);
						});
						break;

					default:
						accept(data);
						break;
				}
			});
		});

		req.on("error", reject);
		req.end();
	});
}

(function* () {
	console.log(yield feeds.parseFeed(yield download("https://kat.cr/movies/?rss=1")));
}).async().catch(error => util.error("main", error));
