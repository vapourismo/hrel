"use strict";

const db = require("./database");

function normalize(name) {
	const segments = name.split("-");
	if (segments.length > 1) segments.pop();

	return segments.join(" ").replace(/([^a-zA-Z0-9])/g, " ").replace(/\s\s+/g, " ").trim().toLowerCase();
}

const table = new db.Table("releases", "id", ["name"]);

const insert = function* (name) {
	yield db.query("INSERT INTO releases (name) SELECT $1 :: varchar WHERE NOT EXISTS (SELECT * FROM releases WHERE name = $1 :: varchar)", [name]);
	return (yield table.find({name})).pop();
}.async;

module.exports = {
	normalize,
	insert
};
