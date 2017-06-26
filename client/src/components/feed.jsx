/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

const page       = require("page");
const React      = require("react");
const superagent = require("superagent");

function fetchReleases(id, callback) {
	superagent.get("/api/feeds/" + id)
		.set("Accept", "application/json")
		.end(function (err, res) {
			if (err) console.error(err);
			else     callback(res);
		});
}

const Release = React.createClass({
	render() {
		const inserted = new Date(this.props.data.inserted).toLocaleString();

		return (
			<div className="row">
				<div className="cell title">{this.props.data.title}</div>
				<div className="cell source">{this.props.data.source}</div>
				<div className="cell inserted">{inserted}</div>
				<a className="cell link" href={this.props.data.uri}>link</a>
				<a className="cell add" target="blank" href={"https://www.premiumize.me/downloader?magnet=" + encodeURIComponent(this.props.data.uri)}>add</a>
			</div>
		);
	}
});

const Feed = React.createClass({
	getInitialState() {
		return {releases: []};
	},

	componentDidMount() {
		fetchReleases(this.props.id, result => {
			if (result.statusCode == 200)
				this.setState({releases: result.body});
		});
	},

	render() {
		const releases = this.state.releases.map(
			release => <Release key={release.id} data={release}/>
		);

		return (
			<div className="feed">
				<div className="table">
					{releases}
				</div>
			</div>
		);
	}
});

module.exports = Feed;
