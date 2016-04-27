module.exports = {
	entry: "./client/source/index.jsx",
	output: {
		filename: "./client/static/index.js"
	},
	module: {
		loaders: [
			{
				test: /\.jsx$/,
				exclude: /node_modules/,
				loader: "babel",
				query: {
					presets: ["react", "es2015"],
					plugins: ["transform-runtime"]
				}
			}
		]
	}
};
