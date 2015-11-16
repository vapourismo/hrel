/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const https = require("https");
const http  = require("http");
const zlib  = require("zlib");
const url   = require("url");

/**
 * Download via HTTP. Also decode the contents.
 * @param {Object or String} options Requested URL or request options
 * @returns {Promise<Buffer>}
 */
function download(options) {
	if (!(options instanceof Object))
		options = url.parse(options);

	let schema = options.protocol == "https:" ? https : http;

	return new Promise(function (accept, reject) {
		const req = schema.request(options, function (res) {
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
