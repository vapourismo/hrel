/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

import React from "react";
import ReactDOM from "react-dom";
import {Router, Route, browserHistory} from "react-router";
import SuperAgent from "superagent";

class JustSearch extends React.Component {
	static get contextTypes () {
		return {
			router: React.PropTypes.object
		};
	}

	constructor(props) {
		super(props);

		this.submitSeach = this.submitSeach.bind(this);
	}

	submitSeach(ev) {
		if (ev)
			ev.preventDefault();

		if (this.inputElem) {
			const queryString = this.inputElem.value.trim();

			if (queryString == "")
				this.inputElem.select();
			else
				this.gotoSearch(queryString);
		}

		return false;
	}

	gotoSearch(queryString) {
		this.context.router.push("/search/" + encodeURIComponent(queryString));
	}

	render() {
		const searchTerm = this.props.params && this.props.params.query
		                  ? this.props.params.query
		                  : "";

		return (
			<div className="content">
				<form className="search-form" onSubmit={this.submitSeach}>
					<input className="input" type="text" ref={elem => this.inputElem = elem}
					       defaultValue={searchTerm} />
					<div className="submit-button" onClick={this.submitSeach}>
						Search
					</div>
				</form>
			</div>
		);
	}
}

class SearchResult extends JustSearch {
	static get contextTypes () {
		return {
			router: React.PropTypes.object
		};
	}

	constructor(props) {
		super(props);

		this.state = {
			searching: props.params && props.params.query,
			results: []
		};

		if (this.state.searching)
			this.performSearch(props.params.query);
	}

	componentWillReceiveProps(nextProps) {
		const newState = {
			searching: nextProps.params && nextProps.params.query,
			results: []
		};

		this.setState(newState);

		if (newState.searching)
			this.performSearch(nextProps.params.query);
	}

	performSearch(query) {
		SuperAgent.get("/api/search")
			.query({q: query})
			.set("Accept", "application/json")
			.end((err, res) => {
				if (!err && res.statusCode == 200 && res.body instanceof Array) {
					this.setState({searching: false, results: res.body});
				} else {
					this.setState({searching: false, results: []});
					console.error(err, res);
				}
			});
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
				{super.render()}
				<div className="results">
					{this.renderResults()}
				</div>
			</div>
		);
	}
}

// Load event
window.addEventListener("load", function () {
	ReactDOM.render(
		<Router history={browserHistory}>
			<Route path="/" component={JustSearch} />
			<Route path="/search/:query" component={SearchResult} />
		</Router>,
		document.getElementById("canvas")
	);
});
