"use strict";

const db    = require("./database");
const util  = require("./utilities");
const http  = require("./http");
const feeds = require("./feeds");

feeds.scan().catch(util.logError);
