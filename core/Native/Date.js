
/*
import Maybe
import JavaScript
*/

(function() {
'use strict';

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(Elm.JavaScript.castStringToJSString(str));
     if (isNaN(d.getTime())) return Elm.Maybe.Nothing;
     return Elm.Maybe.Just(d);
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 Elm.Native.Date = {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return { ctor:monthTable[d.getMonth()] }; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     toTime  : function(d) { return d.getTime(); }
     dayOfWeek : function(d) { return { ctor:dayTable[d.getDay()] }; },
 };

}());
