/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const EventEmitter = require("events");
const config       = require("./config");
const pg           = config.usePGNative ? require("pg").native : require("pg");
const util         = require("./utilities");

// Connect
const db = new pg.Client(config.database || {});
db.connect();

/**
 * Asynchronously query the database.
 * @param {String} query SQL statement
 * @returns {Promise<Object>}
 */
const query = function (...args) {
	return new Promise(function (accept, reject) {
		db.query(...args, function (err, result) {
			if (err) reject(err);
			else     accept(result);
		});
	}.bind(this));
};

/**
 * Terminate the database connection.
 */
function end() {
	db.end();
}

/**
 * Clean a an identifier which shall be used in a SQL statement.
 */
function sanitizeName(name) {
	return '"' + name.replace('"', "") + '"';
}

/**
 * Insert into a table.
 * @param {String}  table Table name
 * @param {Object}  data  Column data
 * @param {Boolean} ret   Return inserted rows (Default: `false`)
 * @returns {Promise<Object>} Inserted row (as seen from the database)
 */
const insert = function* (table, data, ret) {
	const columns = Object.keys(data);
	const values = columns.map(k => data[k]);
	const placeholders = columns.map((_, i) => `$${i + 1}`);

	const returningClause = ret ? "RETURNING *" : "";

	const result = yield query(
		`INSERT INTO ${sanitizeName(table)} (${columns.map(sanitizeName).join(", ")})
		 VALUES (${placeholders.join(", ")}) ${returningClause}`,
		values
	);

	if (ret) return result.rows.pop();
}.async;

/**
 * Insert if unique constraints are not violated.
 * @param {String}               table        Table name
 * @param {Object}               data         Column data
 * @param {Array<Array<String>>} uniqueGroups Unique groups (Disjunctive form)
 * @param {Boolean}              ret          Return inserted rows (Default: `false`)
 * @returns {Promise<Object>} Inserted row (as seen from the database)
 */
const insertUnique = function* (table, data, uniqueGroups, ret) {
	const columns = Object.keys(data), values = [];
	uniqueGroups = uniqueGroups || [columns];

	const placeholders = columns.map(key => {
		values.push(data[key]);
		return `$${values.length}`;
	});

	const uniqueConds = uniqueGroups.map(
		group => group.map(key => {
			values.push(data[key]);
			return `${sanitizeName(key)} = $${values.length}`;
		}).join(" AND ")
	);

	const returningClause = ret ? "RETURNING *" : "";

	const result = yield query(
		`INSERT INTO ${sanitizeName(table)} (${columns.map(sanitizeName).join(", ")})
		 SELECT ${placeholders.join(", ")} WHERE NOT EXISTS (
		 	SELECT * FROM ${sanitizeName(table)}
			WHERE ${uniqueConds.join(" OR ") || "0"}
		 ) ${returningClause}`,
		values
	);

	if (ret) return result.rows.pop();
}.async;

/**
 * Find all rows in a table.
 * @param {String} table Table name
 * @returns {Promise<Array<Object>>} Table rows
 */
const findAll = function* (table) {
	const result = yield query(`SELECT * FROM ${sanitizeName(table)}`);
	return result.rows;
}.async;

/**
 * Find rows that match exactly.
 * @param {String} table    Table name
 * @param {Object} criteria Filter criteria
 * @returns {Promise<Array<Object>>} Matched rows
 */
const findExactly = function* (table, criteria) {
	const columns = Object.keys(criteria);
	const values = columns.map(k => criteria[k]);
	const whereClause = columns.map((k, i) => `${sanitizeName(k)} = $${i + 1}`).join(" AND ");

	const result = yield query(
		`SELECT * FROM ${sanitizeName(table)} WHERE ${whereClause || "1"}`,
		values
	);

	return result.rows;
}.async;

/**
 * Update matching rows.
 * @param {String}  table    Table name
 * @param {Object}  data     Updated data
 * @param {Object}  criteria Update criteria
 * @param {Boolean} ret      Return updated rows (Default: `false`)
 * @returns {Promise}
 */
const updateExactly = function* (table, data, criteria, ret) {
	const values = [];

	const setClauses = Object.keys(data).map(key => {
		values.push(data[key]);
		return `${sanitizeName(key)} = $${values.length}`;
	});

	if (setClauses.length < 1)
		return;

	const whereClauses = Object.keys(criteria).map(key => {
		values.push(criteria[key]);
		return `${sanitizeName(key)} = $${values.length}`;
	});

	const returningClause = ret ? "RETURNING *" : "";

	const result = yield query(
		`UPDATE ${sanitizeName(table)} SET ${setClauses.join(", ")}
		 WHERE ${whereClauses.join(" AND ") || "1"} ${returningClause}`,
		values
	);

	if (ret) return result.rows;
}.async;

/**
 * Delete matching rows.
 * @param {String}  table    Table name
 * @param {Object}  criteria Delete criteria
 * @param {Boolean} ret      Return deleted rows (Default: `false`)
 * @returns {Promise}
 */
const deleteExactly = function* (table, criteria, ret) {
	const columns = Object.keys(criteria);
	const values = columns.map(k => criteria[k]);
	const whereClause = columns.map((k, i) => `${sanitizeName(k)} = $${i + 1}`).join(" AND ");
	const returningClause = ret ? "RETURNING *" : "";

	const result = yield query(
		`DELETE FROM ${sanitizeName(table)} WHERE ${whereClause || "1"} ${returningClause}`,
		values
	);

	if (ret) return result.rows;
}.async;

module.exports = {
	query,
	end,
	insert,
	insertUnique,
	findAll,
	findExactly,
	updateExactly,
	deleteExactly
};
