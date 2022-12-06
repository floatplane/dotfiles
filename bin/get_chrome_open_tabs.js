#!/usr/bin/env osascript -l JavaScript

var app = Application("Google Chrome");
var tabs = app.windows[0].tabs().map((tab) => ({ title: tab.title(), url: tab.url() }));

JSON.stringify(tabs, null, 2);

