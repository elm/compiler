
Elm.Date = function() {

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(Elm.JavaScript.castStringToJSString(str));
     if (isNaN(d.getTime())) return ["Nothing"];
     return ["Just",d];
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 return {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return [monthTable[d.getMonth()]]; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     dayOfWeek : function(d) { return [dayTable[d.getDay()]]; },
     toTime  : function(d) { return d.getTime(); },
     Mon : ["Mon"], Tue : ["Tue"], Wed : ["Wed"],
     Thu : ["Thu"], Fri : ["Fri"], Sat : ["Sat"], Sun : ["Sun"],
     Jan : ["Jan"], Feb : ["Feb"], Mar : ["Mar"], Apr : ["Apr"],
     May : ["May"], Jun : ["Jun"], Jul : ["Jul"], Aug : ["Aug"],
     Sep : ["Sep"], Oct : ["Oct"], Nov : ["Nov"], Dec : ["Dec"]
 };

}();
