Elm.Native.Bitwise = {};
Elm.Native.Bitwise.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Bitwise = elm.Native.Bitwise || {};
    if (elm.Native.Bitwise.values) return elm.Native.Bitwise.values;

    function and(a,b) { return a & b; }
    function or (a,b) { return a | b; }
    function xor(a,b) { return a ^ b; }
    function not(a) { return ~a; }
    function sll(a,offset) { return a << offset; }
    function sra(a,offset) { return a >> offset; }
    function srl(a,offset) { return a >>> offset; }

    return elm.Native.Bitwise.values = {
        and: F2(and),
        or : F2(or ),
        xor: F2(xor),
        complement: not,
        shiftLeft           : F2(sll),
        shiftRightArithmatic: F2(sra),
        shiftRightLogical   : F2(srl)
    };
    
};
