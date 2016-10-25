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
			"http://kickasstorrents.to/highres-movies/?rss=1",
			"http://kickasstorrents.to/movies/?rss=1",
			"http://kickasstorrents.to/tv/?rss=1"
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
		},

		// thepiratebay.org
		thepiratebay: [
			"https://thepiratebay.org/rss/top100/0",
			"https://thepiratebay.org/rss/top100/201",
			"https://thepiratebay.org/rss/top100/205",
			"https://thepiratebay.org/rss/top100/207",
			"https://thepiratebay.org/rss/top100/208"
		],

		// rarbg.to
		rarbg: [
			"https://rarbg.to/rssdd.php?category="
		]
	}
};
