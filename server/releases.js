"use strict";

const db = require("./database");

const table = new db.Table("releases", "id", ["name"]);

const insert = function* (name) {
	try {
		return yield table.insert({name});
	} catch (error) {
		if (error.code == 23505 && error.constraint == "releases_name_key")
			return (yield table.find({name})).pop();
		else
			throw error;
	}
}.async;

module.exports = {
	insert
};
