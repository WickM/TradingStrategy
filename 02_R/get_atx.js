// scrape_techstars.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'atx.html'

page.open('https://de.finance.yahoo.com/quote/%5EATX/components?p=%5EATX', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});