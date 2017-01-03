/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

import React from "react";
import ReactDOM from "react-dom";
import SuperAgent from "superagent";

// Root
class Root extends React.Component {
	constructor(props) {
		super(props);

		this.state = {
			results: [],
			searching: false
		};

		this.submitSeach = this.submitSeach.bind(this);
	}

	performSearch(query) {
		// SuperAgent.post("/api/search")
		// 	.set("Accept", "application/json")
		// 	.set("Content-Type", "text/plain")
		// 	.send(query)
		// 	.end((err, res) => {
		// 		if (!err && res.statusCode == 200 && res.body instanceof Array) {
		// 			this.setState({results: res.body});
		// 		} else {
		// 			console.error(err, res);
		// 		}
		// 	});

		this.setState({results: [], searching: true});

		SuperAgent.get("/api/search")
			.query({q: query})
			.set("Accept", "application/json")
			.end((err, res) => {
				if (!err && res.statusCode == 200 && res.body instanceof Array) {
					this.setState({results: res.body, searching: false});
				} else {
					console.error(err, res);
				}
			});
	}

	submitSeach(ev) {
		if (ev)
			ev.preventDefault();

		if (this.inputElem) {
			const queryString = this.inputElem.value.trim();

			if (queryString == "")
				this.inputElem.select();
			else
				this.performSearch(queryString);
		}

		return false;
	}

	renderResult(result, idx) {
		const downloaderLink = "https://www.premiumize.me/downloader?magnet=" + encodeURIComponent(result.uri);

		return (
			<div className="result" key={idx}>
				<div className="cell title">{result.title}</div>
				<a className="cell link" target="blank" href={result.uri}>link</a>
				<a className="cell add" target="blank" href={downloaderLink}>add</a>
			</div>
		);
	}

	renderResults() {
		if (!this.state.searching) {
			return (
				this.state.results.length > 0
					? this.state.results.map(this.renderResult)
					: <div className="empty">Empty result set</div>
			);
		} else {
			return <div className="empty">Searching ...</div>;
		}
	}

	render() {
		return (
			<div className="content">
				<form className="search-form" onSubmit={this.submitSeach}>
					<input className="input" type="text" ref={elem => this.inputElem = elem} />
					<div className="submit-button" onClick={this.submitSeach}>
						Search
					</div>
				</form>
				<div className="results">
					{this.renderResults()}
				</div>
			</div>
		);
	}
}

// Load event
window.addEventListener("load", function () {
	ReactDOM.render(<Root />, document.getElementById("canvas"));
});
