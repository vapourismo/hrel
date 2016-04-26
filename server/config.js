/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

const fs = require("fs");

const defaultConfig = {
	database: {
		host: "localhost",
		user: "hrel",
		database: "hrel"
	},
	server: {
		host: "127.0.0.1",
		port: 3102,
		exclusive: false
	},
	colorLogging: true,
	usePGNative: true
};

function validateObject(schema, instance) {
	if (typeof(schema) != typeof(instance))
		return schema;

	if (schema instanceof Array) {
		return instance;
	} else if (schema instanceof Object) {
		const object = {};

		for (var key in schema) {
			object[key] = validateObject(schema[key], instance[key]);
		}

		return object;
	} else {
		return instance;
	}
}

try {
	module.exports = validateObject(defaultConfig, JSON.parse(fs.readFileSync("config.json")));
} catch (e) {
	module.exports = defaultConfig;
}
