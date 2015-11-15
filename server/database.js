"use strict";

const EventEmitter = require("events");
const pg           = require("pg").native;
const config       = require("./config");
const util         = require("./utilities");

// Connect
const db = new pg.Client(config.database || {});
db.connect();

// Async query method
const query = function (...args) {
	return new Promise(function (accept, reject) {
		db.query(...args, function (err, result) {
			if (err) reject(err);
			else     accept(result);
		});
	}.bind(this));
};

function end() {
	db.end();
}

function sanitizeName(name) {
	return '"' + name.replace('"', "") + '"';
}

/**
 * Database row
 */
class Row {
	/**
	 * @constructor
	 * @param {Table}  table Containing table
	 * @param {Object} data  Row data
	 */
	constructor(table, data) {
		this.table = table;
		this.data = data;
	}

	// Documentation placeholders

	/**
	 * Update the data connected to this row.
	 * @returns {Promise}
	 * @param {Object} data Incremental row data
	 */
	update(data) {}

	/**
	 * Delete this row from the table.
	 * @returns {Promise}
	 */
	delete() {}
}

Row.prototype.update = function* (data) {
	const key = this.data[this.table.key];

	if (key === undefined)
		throw new Error("Cannot update rows without a primary key");

	const values = [];
	const params = [key];

	this.table.columns.forEach(column => {
		if (!(column in data))
			return;

		params.push(data[column]);
		values.push(sanitizeName(column) + " = $" + params.length);
	});

	const results = yield query(
		"UPDATE " + sanitizeName(this.table.name) + " SET " + values.join(", ") + " WHERE " + sanitizeName(this.table.key) + " = $1 RETURNING *",
		params
	);

	Object.assign(this.data, results.rows.pop());
}.async;

Row.prototype.delete = function* () {
	const key = this.data[this.table.key];

	if (key === undefined)
		throw new Error("Cannot delete rows without a primary key");

	const results = yield query(
		"DELETE FROM " + sanitizeName(this.table.name) + " WHERE " + sanitizeName(this.table.key) + " = $1 RETURNING *",
		[key]
	);

	results.rows.forEach(row => {
		if (row[this.table.key] == key)
			delete this.data[this.table.key];
	});
}.async;

/**
 * Database table
 */
class Table {
	/**
	 * @constructor
	 * @param {String}        name    Table name
	 * @param {String}        key     Primary key name
	 * @param {Array<String>} columns Column names
	 */
	constructor(name, key, columns) {
		this.name = name;
		this.key = key;
		this.columns = columns;
	}

	// Documentation placeholders

	/**
	 * Load the entire data set.
	 * @returns {Promise<Array<Row>>} Array of all rows
	 */
	load() {}

	/**
	 * Insert a new row with the given data.
	 * @param {Object} data Row data
	 * @returns {Promise<Row>} Newly inserted row
	 */
	insert(data) {}

	/**
	 * Find a row with the given criteria.
	 * @param {Object} criteria Search criteria
	 * @returns {Promise<Array<Row>>} Found rows
	 */
	find(criteria) {}
}

Table.prototype.load = function* () {
	const results = yield query("SELECT * FROM " + sanitizeName(this.name));
	return results.rows.map(row => new Row(this, row));
}.async;

Table.prototype.insert = function* (data) {
	const columns = [];
	const values = [];
	const params = [];

	this.columns.forEach(function (column) {
		if (!(column in data))
			return;

		columns.push(sanitizeName(column));
		params.push(data[column]);
		values.push("$" + params.length);
	});

	const results = yield query(
		"INSERT INTO " + sanitizeName(this.name) + " (" + columns.join(", ") + ") VALUES (" + values.join(", ") + ") RETURNING *",
		params
	);

	return new Row(this, results.rows.pop());
}.async;

Table.prototype.find = function* (criteria) {
	const clauses = [];
	const params = [];

	this.columns.forEach(function (column) {
		if (!(column in criteria))
			return;

		params.push(criteria[column]);
		clauses.push(sanitizeName(column) + " = $" + params.length);
	});

	const result = yield query(
		"SELECT * FROM " + sanitizeName(this.name) + " WHERE " + clauses.join(", "),
		params
	);

	return result.rows.map(row => new Row(this, row));
}.async;

Table.prototype.upsert = function* (data, uniqueCols) {
	uniqueCols = uniqueCols || [Object.keys(data)];

	const columns = [];
	const values = [];
	const params = [];

	this.columns.forEach(function (column) {
		if (!(column in data))
			return;

		columns.push(sanitizeName(column));
		params.push(data[column]);
		values.push("$" + params.length);
	});

	const upsertCond = [];

	uniqueCols.forEach(function (uniqueColGroup) {
		const groupCond = [];

		uniqueColGroup.forEach(function (uniqueCol) {
			if (!(uniqueCol in data))
				return;

			params.push(data[uniqueCol]);
			groupCond.push(sanitizeName(uniqueCol) + " = $" + params.length);
		});

		if (groupCond.length > 0)
			upsertCond.push(groupCond.join(" AND "));
	});

	const result = yield query(
		"INSERT INTO " + sanitizeName(this.name) + " (" + columns.join(", ") + ") SELECT " + values.join(", ") + " WHERE NOT EXISTS (SELECT * FROM " + sanitizeName(this.name) + " WHERE " + upsertCond.join(" OR ") + ") RETURNING *",
		params
	);

	return result.rows.map(row => new Row(this, row));
}.async;

module.exports = {
	Row, Table, query, end
};
