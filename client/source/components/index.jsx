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
	openFeed() {
		page("/feed/" + this.props.data.id);
	},

	render() {
		return (
			<div className="feed" onClick={this.openFeed}>
				{this.props.data.uri}
			</div>
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
				<div className="feeds">
					{feeds}
					<Feed data={{id: 1338, uri: "http://short/feed"}}/>
				</div>
			</div>
		);
	}
});

module.exports = Index;
