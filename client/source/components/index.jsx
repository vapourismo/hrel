const page       = require("page");
const React      = require("react");
const superagent = require("superagent");

function fetchFeeds(callback) {
	superagent.get("/api/feeds")
		.set("Accept", "application/json")
		.end(function (err, res) {
			if (err) console.error(err);
			else     callback(res);
		});
}

const Feed = React.createClass({
	render() {
		return (
			<a className="row" href={"/feeds/" + this.props.data.id}>
				<div className="cell title">{this.props.data.title}</div>
				<div className="cell uri">{this.props.data.uri}</div>
				<div className="cell count">{this.props.data.count}</div>
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
				<div className="table">
					{feeds}
				</div>
			</div>
		);
	}
});

module.exports = Index;
