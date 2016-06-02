/* Copyright (C) 2016, Ole Krüger <ole@vprsm.de> */

"use strict";

const db   = require("./database");
const util = require("./utilities");

const insert = function* (title, uri, release, source) {
	if (!title || !release || !uri)
		return 0;

	const result = yield db.query(
		"INSERT INTO links (title, uri, release, source) SELECT $1 :: varchar, $2 :: varchar, r.id, $4 :: varchar FROM releases r WHERE r.name = $3 :: varchar AND NOT EXISTS (SELECT * FROM links WHERE uri = $2 :: varchar) RETURNING id",
		[title, uri, release, source]
	);

	return result.rows.length;
}.async;

module.exports = {
	insert
};
