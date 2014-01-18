Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Trampoline = elm.Native.Trampoline || {};
    if (elm.Native.Trampoline.values) return elm.Native.Trampoline.values;

    function trampoline(thunkstack,next){
     var next_ts;
     while((next_ts=next(thunkstack)) && next_ts.ctor=="Left"){
      thunkstack=next_ts._0;
     }//Loop ends when next_ts is Right
     return next_ts._0;
    }

    return elm.Native.Trampoline.values = {
        trampoline: F2(trampoline),
    };
    
};
