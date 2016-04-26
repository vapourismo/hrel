/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const https = require("https");
const http  = require("http");
const zlib  = require("zlib");
const url   = require("url");
const util  = require("./utilities");

/**
 * Generate a HTTP request and process its result.
 * @param {Object or String} options Requested URL or request options
 * @param {Function} accept  Invoked when the response object is valid and has status code 200
 * @param {Function} reject  Invoked upon error
 */
function makeRequest(options, accept, reject, redirects) {
	redirects = redirects || 0;

	if (!(options instanceof Object))
		options = url.parse(options);

	let schema = options.protocol == "https:" ? https : http;

	const req = schema.request(options, function (response) {
		processResponse(response, accept, reject, redirects);
	});

	req.on("error", reject);
	req.end();
}

/**
 * Process a HTTP response. This function will follow up to 10 redirections.
 * @param {Response} response  Response object
 * @param {Function} accept    Invoked when the response object is valid and has status code 200
 * @param {Function} reject    Invoked upon error
 * @param {Number}   redirects Number of redirects which have been followed so far
 */
function processResponse(response, accept, reject, redirects) {
	if (redirects >= 10)
		return reject(new Error("Maximum number of redirections exceeded"));

	switch (response.statusCode) {
		case 200:
			accept(response);
			break;

		case 301:
			if ("location" in response.headers)
				makeRequest(response.headers["location"], accept, reject, redirects + 1);
			else
				reject(new Error("Response is missing a location header"));

			break;

		default:
			reject(new Error("Invalid response status code (" + response.statusMessage + ")"));
			break;
	}
}

/**
 * Wrap a HTTP request in a Promize.
 * @param {Object or String} options Requested URL or request options
 * @returns {Promise<Response>}
 */
function request(options) {
	return new Promise(function (accept, reject) {
		makeRequest(options, accept, reject, 0);
	});
}

/**
 * Download via HTTP. Also decode the contents.
 * @param {Object or String} options Requested URL or request options
 * @returns {Promise<Buffer>}
 */
const download = function* (options, maxContentLength) {
	const res = yield request(options);

	if (maxContentLength && "content-length" in res.headers && res.headers["content-length"] > maxContentLength)
		return reject(new Error("Content too long"));

	return yield new Promise(function (accept, reject) {
		let received = 0;
		const chunks = [];

		res.on("data", function (chunk) {
			if (maxContentLength && received + chunk.length > maxContentLength) {
				res.removeAllListeners();
				return reject(new Error("Content too long"));
			}

			chunks.push(chunk);
			received += chunk.length;
		});

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
}.async;

const stream = function* (options) {
	const res = yield request(options);

	switch (res.headers["content-encoding"]) {
		case "gzip":    return res.pipe(zlib.createGunzip());
		case "deflate": return res.pipe(zlib.createInflate());
		default:        return res;
	}
}.async;

module.exports = {
	download,
	stream
};
