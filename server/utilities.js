/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const stream = require("stream");
const fs     = require("fs");
const path   = require("path");
const config = require("./config");

/**
 * Interate over every file in a directory.
 * @param {String}   base     Directory path
 * @param {Function} callback Interator function
 */
function iterateFiles(base, callback) {
	fs.readdirSync(base).forEach(function (entry) {
		entry = path.join(base, entry);
		const stat = fs.statSync(entry);

		if (stat.isFile()) {
			callback(entry);
		} else if (stat.isDirectory()) {
			iterateFiles(entry, callback);
		}
	});
}

/**
 * Split a buffer at a given symbol (byte value).
 * @param {Buffer} buffer Input buffer
 * @param {Number} symbol Byte value to split at
 * @returns {Array<Buffer>} Slices of the original buffer
 */
function splitBuffer(buffer, symbol) {
	const length = buffer.length;
	const chunks = [];
	let start = 0;

	for (let i = 0; i < length; i++) {
		if (buffer[i] == symbol) {
			chunks.push(buffer.slice(start, i));
			start = i + 1;
		}
	}

	if (start <= length)
		chunks.push(buffer.slice(start, length));

	return chunks;
}

/**
 * Split streamed data
 */
class Splitter extends stream.Transform {
	constructor(symbol) {
		super();

		this.latest = new Buffer(0);
		this.symbol = symbol;
	}

	_transform(chunk, _, done) {
		const segments = splitBuffer(Buffer.concat([this.latest, chunk]), this.symbol);
		this.latest = segments.pop();

		segments.forEach(c => this.push(c));
		done();
	}

	_flush(done) {
		splitBuffer(this.latest, this.symbol).forEach(c => this.push(c));
		done();
	}
}

/**
 * Validate the schema of a value.
 * @param {*} schema Schema
 * @param {*} input  Value
 */
function validateSchema(schema, input) {
	switch (typeof(schema)) {
		case "string":
			return typeof(input) == schema;

		case "array":
			if (typeof(input) != "array")
				return false;

			return input.all(e => schema.some(t => validateSchema(t, e)));

		case "object":
			if (typeof(input) != "object")
				return false;

			for (let key in schema) {
				if (!(key in input) || !validateSchema(schema[key], input[key]))
				    return false;
			}

			return true;

		default:
			return false;
	}
}

/*
 * Logging functions
 */

function informColor(tag, ...msg) {
	console.log("\x1b[32mI\x1b[0m", "\x1b[34m[" + tag + "]\x1b[0m", ...msg);
}

function informPlain(tag, ...msg) {
	console.log("I [" + tag + "]", ...msg);
}

function warnColor(tag, ...msg) {
	console.warn("\x1b[33mW\x1b[0m", "\x1b[34m[" + tag + "]\x1b[0m", ...msg);
}

function warnPlain(tag, ...msg) {
	console.warn("W [" + tag + "]", ...msg);
}

function errorColor(tag, ...msg) {
	console.error("\x1b[31mE\x1b[0m", "\x1b[34m[" + tag + "]\x1b[0m", ...msg);
}

function errorPlain(tag, ...msg) {
	console.error("E [" + tag + "]", ...msg);
}

function debugColor(tag, ...msg) {
	console.log("\x1b[35mD\x1b[0m", "\x1b[34m[" + tag + "]\x1b[0m", ...msg);
}

function debugPlain(tag, ...msg) {
	console.log("D [" + tag + "]", ...msg);
}

// Decide which loggers shall be used
var loggers = config.colorLogging ? {error: errorColor, warn: warnColor, inform: informColor, debug: debugColor}
                                  : {error: errorPlain, warn: warnPlain, inform: informPlain, debug: debugPlain};

/**
 * Log an instance of 'Error'.
 */
function logError(error) {
	loggers.error("error", error instanceof Error ? error.stack : error);
}

/*
 * Generator utilities
 */

const GeneratorProto = (function* () {}).__proto__;

function serve(iter, step, accept, reject) {
	if (step.done) {
		accept(step.value);
	} else {
		step.value.then(
			function (value) {
				try {
					serve(iter, iter.next(value), accept, reject);
				} catch (error) {
					reject(error);
				}
			},
			function (error) {
				try {
					logError(error);
					serve(iter, iter.throw(error), accept, reject);
				} catch (error) {
					reject(error);
				}
			}
		);
	}
}

// GeneratorFunction#async property
Object.defineProperty(GeneratorProto, "async", {
	get: function () {
		const generator = this;

		return function (...args) {
			return new Promise((accept, reject) => {
				const iter = generator.call(this, ...args);
				serve(iter, iter.next(), accept, reject);
			});
		};
	},
	enumerable: false
});

/*
 * Object utilities
 */

// Object#forEach method
Object.defineProperty(Object.prototype, "forEach", {
	value: function (callback) {
		for (let key in this) callback(key, this[key]);
	},
	enumerable: false,
	writable: true
});

// Object#map method
Object.defineProperty(Object.prototype, "map", {
	value: function (callback) {
		let newObject = {};

		for (let key in this)
			newObject[key] = callback(key, this[key]);

		return newObject;
	},
	enumerable: false,
	writable: true
});

/*
 * Exports
 */

module.exports = {
	iterateFiles,
	validateSchema,
	logError,

	splitBuffer,
	Splitter
};

Object.assign(module.exports, loggers);
