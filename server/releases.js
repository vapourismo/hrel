"use strict";

const db = require("./database");

const table = new db.Table("releases", "id", ["name"]);

const insert = function* (name) {
	yield db.query("INSERT INTO releases (name) SELECT $1 :: varchar WHERE NOT EXISTS (SELECT * FROM releases WHERE name = $1 :: varchar)", [name]);
	return (yield table.find({name})).pop();
}.async;

module.exports = {
	insert
};
