
/*
 * Copyright (c) 2010 Mozilla Corporation
 * Copyright (c) 2010 Vladimir Vukicevic
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * File: mjs
 *
 * Vector and Matrix math utilities for JavaScript, optimized for WebGL.
 * Edited to work with the Elm Programming Language
 */

Elm.Native.Math = Elm.Native.Math || {};
Elm.Native.Math.Vector4 = {};
Elm.Native.Math.Vector4.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Math = elm.Native.Math || {};
    elm.Native.Math.Vector4 = elm.Native.Math.Vector4 || {};
    if (elm.Native.Math.Vector4.values) return elm.Native.Math.Vector4.values;

    var MJS_FLOAT_ARRAY_TYPE = Float32Array;

    var V4 = { };

    if (MJS_FLOAT_ARRAY_TYPE == Array) {
        V4.$ = function V4_$(x, y, z, w) {
            return [x, y, z, w];
        };
    } else {
        V4.$ = function V4_$(x, y, z, w) {
            return new MJS_FLOAT_ARRAY_TYPE([x, y, z, w]);
        };
    }

    V4.getX = function V4_getX(a) {
        return a[0];
    }
    V4.getY = function V4_getY(a) {
        return a[1];
    }
    V4.getZ = function V4_getZ(a) {
        return a[2];
    }
    V4.getW = function V4_getW(a) {
        return a[3];
    }
    V4.setX = function V4_setX(x, a) {
        return new MJS_FLOAT_ARRAY_TYPE(x, a[1], a[2], a[3]);
    }
    V4.setY = function V4_setY(y, a) {
        return new MJS_FLOAT_ARRAY_TYPE(a[0], y, a[2], a[3]);
    }
    V4.setZ = function V4_setZ(z, a) {
        return new MJS_FLOAT_ARRAY_TYPE(a[0], a[1], z, a[3]);
    }
    V4.setW = function V4_setW(w, a) {
        return new MJS_FLOAT_ARRAY_TYPE(a[0], a[1], a[2], w);
    }

    V4.toTuple = function V4_toTuple(a) {
        return {
            ctor:"_Tuple4",
            _0:a[0],
            _1:a[1],
            _2:a[2],
            _3:a[3]
        };
    };
    V4.fromTuple = function V4_fromTuple(t) {
        return new MJS_FLOAT_ARRAY_TYPE([t._0, t._1, t._2, t._3]);
    };

    V4.toRecord = function V4_toRecord(a) {
        return {
            _:{},
            x:a[0],
            y:a[1],
            z:a[2],
            w:a[3]
        };
    };
    V4.fromRecord = function V4_fromRecord(r) {
        return new MJS_FLOAT_ARRAY_TYPE([r.x, r.y, r.z, r.w]);
    };

    V4.add = function V4_add(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        r[0] = a[0] + b[0];
        r[1] = a[1] + b[1];
        r[2] = a[2] + b[2];
        r[3] = a[3] + b[3];
        return r;
    };

    V4.sub = function V4_sub(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        r[0] = a[0] - b[0];
        r[1] = a[1] - b[1];
        r[2] = a[2] - b[2];
        r[3] = a[3] - b[3];
        return r;
    };

    V4.neg = function V4_neg(a) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        r[0] = - a[0];
        r[1] = - a[1];
        r[2] = - a[2];
        r[3] = - a[3];
        return r;
    };

    V4.direction = function V4_direction(a, b) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        r[0] = a[0] - b[0];
        r[1] = a[1] - b[1];
        r[2] = a[2] - b[2];
        r[3] = a[3] - b[3];
        var im = 1.0 / V4.length(r);
        r[0] = r[0] * im;
        r[1] = r[1] * im;
        r[2] = r[2] * im;
        r[3] = r[3] * im;
        return r;
    };

    V4.length = function V4_length(a) {
        return Math.sqrt(a[0]*a[0] + a[1]*a[1] + a[2]*a[2] + a[3]*a[3]);
    };

    V4.lengthSquared = function V4_lengthSquared(a) {
        return a[0]*a[0] + a[1]*a[1] + a[2]*a[2] + a[3]*a[3];
    };

    V4.distance = function V4_distance(a, b) {
        var dx = a[0] - b[0];
        var dy = a[1] - b[1];
        var dz = a[2] - b[2];
        var dw = a[3] - b[3];
        return Math.sqrt(dx * dx + dy * dy + dz * dz + dw * dw);
    };

    V4.distanceSquared = function V4_distanceSquared(a) {
        var dx = a[0] - b[0];
        var dy = a[1] - b[1];
        var dz = a[2] - b[2];
        var dw = a[3] - b[3];
        return dx * dx + dy * dy + dz * dz + dw * dw;
    };

    V4.normalize = function V4_normalize(a) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        var im = 1.0 / V4.length(a);
        r[0] = a[0] * im;
        r[1] = a[1] * im;
        r[2] = a[2] * im;
        r[3] = a[3] * im;
        return r;
    };

    V4.scale = function V4_scale(a, k) {
        var r = new MJS_FLOAT_ARRAY_TYPE(4);
        r[0] = a[0] * k;
        r[1] = a[1] * k;
        r[2] = a[2] * k;
        r[3] = a[3] * k;
        return r;
    };

    V4.dot = function V4_dot(a, b) {
        return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3];
    };

    return { 
        v4: F4(V4.$),
        getX: V4.getX,
        getY: V4.getY,
        getZ: V4.getZ,
        getW: V4.getW,
        setX: F2(V4.setX),
        setY: F2(V4.setY),
        setZ: F2(V4.setZ),
        setW: F2(V4.setW),
        toTuple: V4.toTuple,
        toRecord: V4.toRecord,
        fromTuple: V4.fromTuple,
        fromRecord: V4.fromRecord,
        add: F2(V4.add),
        sub: F2(V4.sub),
        neg: V4.neg,
        direction: F2(V4.direction),
        length: V4.length,
        lengthSquared: V4.lengthSquared,
        distance: F2(V4.distance),
        distanceSquared: F2(V4.distanceSquared),
        normalize: V4.normalize,
        scale: F2(V4.scale),
        dot: F2(V4.dot)
    };

}
