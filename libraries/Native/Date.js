Elm.Native.Date = {};
Elm.Native.Date.make = function(elm) {
 elm.Native = elm.Native || {};
 elm.Native.Date = elm.Native.Date || {};
 if (elm.Native.Date.values) return elm.Native.Date.values;

 var Maybe = Elm.Maybe.make(elm);

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(str);
     if (isNaN(d.getTime())) return Maybe.Nothing;
     return Maybe.Just(d);
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 return elm.Native.Date.values = {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return { ctor:monthTable[d.getMonth()] }; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     toTime  : function(d) { return d.getTime(); },
     fromTime: function(t) { return new window.Date(t); },
     dayOfWeek : function(d) { return { ctor:dayTable[d.getDay()] }; }
 };

};
