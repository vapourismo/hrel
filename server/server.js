/* Copyright (C) 2015, Ole Krüger <ole@vprsm.de> */

"use strict";

const Express = require("express");

var server = Express();

server.get("/", function (req, res) {
	res.send("Hello World!");
});

server.listen(3102, "127.0.0.1");
