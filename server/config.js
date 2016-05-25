/* Copyright (C) 2015-2016, Ole Krüger <ole@vprsm.de> */

module.exports = {
	// Database settings (passed to pg.Client)
	database: {
		host: "localhost",
		user: "hrel",
		database: "hrel"
	},

	// Web server settings (passed to http.Server#listen)
	server: {
		host: "127.0.0.1",
		port: 3102,
		exclusive: false
	},

	// Output coloured logs?
	colorLogging: true,

	// Use 'pg-native' instead of the 'pg' package
	usePGNative: true,

	// Torrent dumps
	sources: {
		// kat.cr
		kat: [
		],

		// movie-blog.org
		movieblog: {
			url: "http://www.movie-blog.org/feed/",
			allowedHosts: [
				"ul.to",
				"www.ul.to",
				"filecrypt.cc",
				"www.filecrypt.cc",
				"relink.us",
				"www.relink.us"
			]
		}
	}
};
