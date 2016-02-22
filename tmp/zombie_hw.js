var Browser = require("zombie");
var assert = require("assert");

// Load the page from localhost
browser = new Browser()
console.log("Hello World1");
browser.visit("http://google.com/", function () {
  // console.log(browser.html());
  // var all = browser.getElementsByTagName("*");
  // var all = browser.querySelectorAll("*")
  // console.log(all._nodeName)
  console.log(browser.document._childNodes[0]._ownerDocument)
//   // Fill email, password and submit form
//   browser.
//     fill("email", "zombie@underworld.dead").
//     fill("password", "eat-the-living").
//     pressButton("Sign Me Up!", function() {
//       // Form submitted, new page loaded.
//       assert.ok(browser.success);
//       assert.equal(browser.text("title"), "Welcome To Brains Depot");
//     });
});

console.log("Hello World2");
