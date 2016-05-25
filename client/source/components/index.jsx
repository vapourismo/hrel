/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

const page       = require("page");
const React      = require("react");
const superagent = require("superagent");

function fetchFeeds(callback) {
	superagent.get("/api/feeds")
		.accept("application/json")
		.end(function (err, res) {
			if (err) console.error(err);
			else     callback(res);
		});
}

function addFeed(uri) {
	return superagent.post("/api/feeds")
		.accept("application/json")
		.type("application/json")
		.send({uri});
}

const InsertForm = React.createClass({
	getInitialState() {
		return {waiting: false, error: null};
	},

	focusInput() {
		this.refs.uri.select();
	},

	submitInput(ev) {
		const uri = this.refs.uri.value.trim();

		if (uri) {
			this.setState({waiting: true, error: null});

			addFeed(uri).then(
				res => {
					this.setState({waiting: false, error: null});
					page("/feeds/" + res.body.id);
				},
				err => {
					const res = err.response;

					if (res.statusCode == 400)
						this.setState({waiting: false, error: res.body.error});
					else
						this.setState({waiting: false, error: "Something went wrong"});
				}
			);
		}

		if (ev) ev.preventDefault();
		return false;
	},

	render() {

		if (this.state.waiting) {
			return (
				<div className="insert">
					<div className="waiting">
						Processing request ...
					</div>
				</div>
			);
		} else {
			let error = null;

			if (this.state.error)
				error = <div className="error">{this.state.error}</div>;

			return (
				<form className="insert" onSubmit={this.submitInput}>
					<input className="uri" type="text" ref="uri"
					       defaultValue="Insert URI here and press Enter"
					       onFocus={this.focusInput}/>
					{error}
				</form>
			);
		}
	}
});

const Feed = React.createClass({
	render() {
		return (
			<a className="row" href={"/feeds/" + this.props.data.id}>
				<div className="cell title">{this.props.data.title}</div>
				<div className="cell uri">{this.props.data.uri}</div>
				<div className="cell count">{this.props.data.nlinks} links</div>
				<div className="cell count">{this.props.data.nreleases} releases</div>
			</a>
		);
	}
});

const Index = React.createClass({
	getInitialState() {
		return {feeds: []};
	},

	componentDidMount() {
		fetchFeeds(result => {
			if (result.statusCode == 200)
				this.setState({feeds: result.body});
		});
	},

	render() {
		const feeds = this.state.feeds.map(feed => <Feed key={feed.id} data={feed}/>);

		return (
			<div className="index">
				<InsertForm />
				<div className="table">
					{feeds}
				</div>
			</div>
		);
	}
});

module.exports = Index;
