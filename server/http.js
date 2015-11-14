"use strict";

const http  = require("http");
const https = require("https");
const zlib  = require("zlib");

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

module.exports = {
	download
};
