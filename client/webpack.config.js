module.exports = {
	entry: "./src/index.jsx",
	output: {
		filename: "./out/index.js"
	},
	module: {
		loaders: [
			{
				test: /\.jsx$/,
				exclude: /node_modules/,
				loader: "babel-loader",
				query: {
					presets: ["react", "es2015"],
					plugins: ["transform-runtime"]
				}
			}
		]
	}
};
