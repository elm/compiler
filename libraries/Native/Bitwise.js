Elm.Native.Bits = {};
Elm.Native.Bits.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Bits = elm.Native.Bits || {};
    if (elm.Native.Bits.values) return elm.Native.Bits.values;

    function and(a,b) { return a & b; }
    function or (a,b) { return a | b; }
    function xor(a,b) { return a ^ b; }
    function not(a) { return ~a; }
    function sll(a,offset) { return a << offset; }
    function sra(a,offset) { return a >> offset; }
    function srl(a,offset) { return a >>> offset; }

    return elm.Native.Bits.values = {
        and: A2(and),
        or : A2(or ),
        xor: A2(xor),
        complement: not,
        shiftLeft           : A2(sll),
        shiftRightArithmatic: A2(sra),
        shiftRightLogical   : A2(srl),
    };
    
};
