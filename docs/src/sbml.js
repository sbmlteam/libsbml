/*
 * From MediaWiki 1.11's common/Common.js
 *
 * Test if an element has a certain class
 *
 * Description: Uses regular expressions and caching for better performance.
 * Maintainers: [[User:Mike Dillon]], [[User:R. Koot]], [[User:SG]]
 */
 
var hasClass = (
  function () {
    var reCache = {};
    return function (element, className) {
      return (reCache[className]
	      ? reCache[className]
	      : (reCache[className]
		 = new RegExp("(?:\\s|^)"
			      + className
			      + "(?:\\s|$)"))).test(element.className);
    };
  }
)();


/*
 * From MediaWiki 1.11's wikibits.js
 */
function hookEvent(hookName, hookFunct) {
  if (window.addEventListener) {
    window.addEventListener(hookName, hookFunct, false);
  } else if (window.attachEvent) {
    window.attachEvent("on" + hookName, hookFunct);
  }
}


/*
 * From www.surfmind.com/musings/2003/09/15
 * Copied 2007-12-12 and modified slightly.
 */

function alternateRowColors()
{
  var className = 'alt-row-colors';
  var rowcolor = '#f3f3f3';
  var rows, arow;
  var tables = document.getElementsByTagName("table");
  var rowCount = 0;
  for (var i = 0; i < tables.length; i++) {
    if (hasClass(tables.item(i), className)) {
      atable = tables.item(i);
      rows = atable.getElementsByTagName("tr");
      for (var j = 0; j < rows.length; j++) {
        arow = rows.item(j);
        if (arow.nodeName == "TR") {
          if (rowCount % 2) {
            // default case
          } else {
            arow.style.backgroundColor = rowcolor;
          }
          rowCount++;
        }
      }
      rowCount = 0;
    }
  }
}

hookEvent("load", alternateRowColors);

