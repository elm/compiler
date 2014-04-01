var jsdom = require("jsdom");
var callback = function(errors, window) {
    var document = window.document;
// Elm goes here:
'use strict';
var Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};
var ElmRuntime = {}; ElmRuntime.Render = {};
Elm.Native.JavaScript = {};
Elm.Native.JavaScript.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.JavaScript = elm.Native.JavaScript || {};
    if (elm.Native.JavaScript.values) return elm.Native.JavaScript.values;

    var List = Elm.Native.List.make(elm);
    var Render = ElmRuntime.use(ElmRuntime.Render.Element);

    function fromJS(v) {
        var type = typeof v;
        if (type === 'number' ) return v;
        if (type === 'boolean') return v;
        if (type === 'string' ) return v;
        if (v instanceof Array) {
            var arr = [];
            var len = v.length;
            for (var i = 0; i < len; ++i) {
                var x = fromJS(v[i]);
                if (x !== null) arr.push(x);
            }
            return List.fromArray(arr);
        }
        if (type === 'object') {
            var rec = { _:{} };
            for (var f in v) {
                var x = fromJS(v[f]);
                if (x !== null) rec[f] = x;
            }
            return rec;
        }
        return null;
    }

    function toJS(v) {
        var type = typeof v;
        if (type === 'number' || type === 'boolean' || type === 'string') return v;
        if (type === 'object' && '_' in v) {
            var obj = {};
            for (var k in v) {
                var x = toJS(v[k]);
                if (x !== null) obj[k] = x;
            }
            return obj;
        }
        if (type === 'object' && (v.ctor === '::' || v.ctor === '[]')) {
            var array = List.toArray(v);
            for (var i = array.length; i--; ) {
                array[i] = toJS(array[i]);
            }
            return array;
        }
        return null;
    }

    function fromRecord(r) {
        if (typeof r === 'object' && '_' in r) {
            return toJS(r);
        }
        throw new Error("'fromRecord' must be called on a record.");
    }

    return elm.Native.JavaScript.values = {
        toRecord    : fromJS,
        fromRecord  : fromRecord
    };

};
Elm.Native.Regex = {};
Elm.Native.Regex.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Regex = elm.Native.Regex || {};
    if (elm.Native.Regex.values) return elm.Native.Regex.values;
    if ('values' in Elm.Native.Regex)
        return elm.Native.Regex.values = Elm.Native.Regex.values;

    var List = Elm.Native.List.make(elm);
    var Maybe = Elm.Maybe.make(elm);

    function escape(str) {
        return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    }
    function caseInsensitive(re) {
        return new RegExp(re.source, 'gi');
    }
    function regex(raw) {
        return new RegExp(raw, 'g');
    }

    function contains(re, string) {
        return string.match(re) !== null;
    }

    function find(n, re, str) {
        n = n.ctor === "All" ? Infinity : n._0;
        var out = [];
        var number = 0;
        var string = str;
        var result;
        while (number++ < n && (result = re.exec(string))) {
            var i = result.length - 1;
            var subs = new Array(i);
            while (i > 0) {
                var submatch = result[i];
                subs[--i] = submatch === undefined
                    ? Maybe.Nothing
                    : Maybe.Just(submatch);
            }
            out.push({
                _:{},
                match: result[0],
                submatches: List.fromArray(subs),
                index: result.index,
                number: number,
            });
        }
        return List.fromArray(out);
    }

    function replace(n, re, replacer, string) {
        n = n.ctor === "All" ? Infinity : n._0;
        var count = 0;
        function jsReplacer(match) {
            if (count++ > n) return match;
            var i = arguments.length-3;
            var submatches = new Array(i);
            while (i > 0) {
                var submatch = arguments[i];
                submatches[--i] = submatch === undefined
                    ? Maybe.Nothing
                    : Maybe.Just(submatch);
            }
            return replacer({
                _:{},
                match:match,
                submatches:List.fromArray(submatches),
                index:arguments[i-1],
                number:count
            });
        }
        return string.replace(re, jsReplacer);
    }

    function split(n, re, str) {
        if (n === Infinity) {
            return List.fromArray(string.split(re));
        }
        var string = str;
        var result;
        var out = [];
        var start = re.lastIndex;
        while (n--) {
            if (!(result = re.exec(string))) break;
            out.push(string.slice(start, result.index));
            start = re.lastIndex;
        }
        out.push(string.slice(start));
        return List.fromArray(out);
    }

    return Elm.Native.Regex.values = {
        regex: regex,
        caseInsensitive: caseInsensitive,
        escape: escape,

        contains: F2(contains),
        find: F3(find),
        replace: F4(replace),
        split: F3(split),
    };
};
﻿Elm.Native.Array = {};
Elm.Native.Array.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Array = elm.Native.Array || {};
    if (elm.Native.Array.values) return elm.Native.Array.values;
    if ('values' in Elm.Native.Array)
      return elm.Native.Array.values = Elm.Native.Array.values;

    var List = Elm.Native.List.make(elm);

    // A RRB-Tree has two distinct data types.
    // Leaf -> "height"  is always 0
    //         "table"   is an array of elements
    // Node -> "height"  is always greater than 0
    //         "table"   is an array of child nodes
    //         "lengths" is an array of accumulated lengths of the child nodes

    // M is the maximal table size. 32 seems fast. E is the allowed increase
    // of search steps when concatting to find an index. Lower values will 
    // decrease balancing, but will increase search steps.
    var M = 32;
    var E = 2;

    // An empty array.
    var empty = { ctor:"_Array", height:0, table:new Array() };

    // Gets the value at index i recursively.
    function get(i, a) {
      if (a.height == 0) {
        if (i < a.table.length) {
          return a.table[i];
        } else {
          throw new Error("Index "+ i +" is out of range. Check the length of your array first or use getSafe or getMaybe.");
        }
      }

      var slot = getSlot(i, a);
      var sub = slot > 0 ? a.lengths[slot-1] : 0;
      return get(i - sub, a.table[slot]);
    }

    // Sets the value at the index i. Only the nodes leading to i will get
    // copied and updated.
    function set(i, item, a) {
      if (length(a) <= i) {
        return a;
      }
      var newA = nodeCopy(a);
      newA.table = a.table.slice();

      if (a.height == 0) {
        newA.table[i] = item;
      } else {
        var slot = getSlot(i, a);
        var sub = slot > 0 ? a.lengths[slot-1] : 0;
        newA.table[slot] = set(i - sub, item, a.table[slot]);
      }
      return newA;
    }

    // Pushes an item via push_ to the bottom right of a tree.
    function push(item, a) {
      var pushed = push_(item, a);
      if (pushed !== null) {
        return pushed;
      }

      newTree = create(item, a.height);
      return siblise(a, newTree);
    }

    // Recursively tries to push an item to the bottom-right most
    // tree possible. If there is no space left for the item,
    // null will be returned.
    function push_(item, a) {
      // Handle resursion stop at leaf level.
      if (a.height == 0) {
        if (a.table.length < M) {
          var newA = { ctor:"_Array", height:0, table:a.table.slice() };
          newA.table.push(item);
          return newA;
        } else {
          return null;
        }
      }

      // Recursively push
      var pushed = push_(item, botRight(a));

      // There was space in the bottom right tree, so the slot will
      // be updated.
      if (pushed != null) {
        var newA = nodeCopy(a);
        newA.table[newA.table.length - 1] = pushed;
        newA.lengths[newA.lengths.length - 1]++;
        return newA
      }

      // When there was no space left, check if there is space left
      // for a new slot with a tree which contains only the item
      // at the bottom.
      if (a.table.length < M) {
        var newSlot = create(item, a.height - 1);
        var newA = nodeCopy(a);
        newA.table.push(newSlot);
        newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
        return newA
      } else {
        return null;
      }
    }

    // Converts an array into a list of elements.
    function toList(a) {
      return toList_(List.Nil, a);
    }

    function toList_(list, a) {
      for (var i = a.table.length - 1; i >= 0; i--) {
        list = a.height == 0 ? List.Cons(a.table[i], list) : toList_(list, a.table[i]);
      }
      return list;
    }

    // Maps a function over the elements of an array.
    function map(f, a) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? f(a.table[i]) : map(f, a.table[i]);
      }
      return newA;
    }

    // Maps a function over the elements with their index as first argument.
    function indexedMap(f, a) {
      var newA = { ctor:"_Array", height:a.height, table:new Array(a.table) };
      if (a.height > 0) { newA.lengths = a.lengths; }
      for (var i = 0; i < a.table.length; i++) {
        newA.table[i] = a.height == 0 ? A2(f, i, a.table[i]) : indexedMap(f, a.table[i]);
      }
      return newA;
    }

    function foldl(f, b, a) {
      for (var i = a.table.length - 1; i >= 0; i--) {
        b = A2(f, a.height == 0 ? a.table[i] : foldl(f, b, a.table[i]), b);
      }
      return b;
    }

    function foldr(f, b, a) {
      for (var i = 0; i < a.table.length; i++) {
        b = A2(f, a.height == 0 ? a.table[i] : foldr(f, b, a.table[i]), b);
      }
      return b;
    }

    // Returns a sliced tree. "to" is inclusive, but this may change,
    // when I understand, why e.g. JS does not handle it this way. :-)
    // If "from" or "to" is negative, they will select from the end on.
    // TODO: currently, it slices the right, then the left. This can be
    // optimized.
    function slice(from, to, a) {
      if (from < 0) { from += length(a); }
      if (to < 0)   { to += length(a); }
      return sliceLeft(from, sliceRight(to, a));
    }

    function sliceRight(to, a) {
      if (to == length(a)) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(0, to + 1);
        return newA;
      }

      // Slice the right recursively.
      var right = getSlot(to, a);
      var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (right == 0) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(0, right + 1)
                                , lengths:a.lengths.slice(0, right + 1) };
      newA.table[right] = sliced;
      newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
      return newA;
    }

    function sliceLeft(from, a) {
      if (from == 0) {
        return a;
      }

      // Handle leaf level.
      if (a.height == 0) {
        var newA = { ctor:"_Array", height:0 };
        newA.table = a.table.slice(from, a.table.length + 1);
        return newA;
      }

      // Slice the left recursively.
      var left = getSlot(from, a);
      var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (left == a.table.length - 1) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice(left, a.table.length + 1)
                                , lengths:new Array(a.table.length - left) };
      newA.table[left] = sliced;
      var len = 0;
      for (var i = 0; i < newA.table.length; i++) {
        len += length(newA.table[i]);
        newA.lengths[i] = len;
      }

      return newA;
    }

    // Concats two trees.
    // TODO: Add support for concatting trees of different sizes. Current
    // behavior will just rise the lower tree and then concat them.
    function concat(a, b) {
      if (b.height > a.height) { return concat(parentise(a, b.height), b); }
      if (a.height > b.height) { return concat(a, parentise(b, a.height)); }
      if (a.height == 0) { return concat(parentise(a, 1), parentise(b, 1)); }

      var c = concat_(a, b);
      if (c[1].table.length > 0) {
        return siblise(c[0], c[1]);
      } else {
        return c[0];
      }
    }

    // Returns an array of two nodes. The second node _may_ be empty. This case
    // needs to be handled by the function, that called concat_. May be only
    // called for trees with an minimal height of 1.
    function concat_(a, b) {
      if (a.height == 1) {
        // Check if balancing is needed and return based on that.
        var toRemove = calcToRemove(a, b);
        if (toRemove <= E) {
          return [a,b];
        }

        return shuffle(a, b, toRemove);
      }

      var concated = concat_(botRight(a), botLeft(b));
      a = nodeCopy(a), b = nodeCopy(b);

      // Adjust the bottom right side of the new tree.
      a.table[a.table.length - 1] = concated[0];
      a.lengths[a.lengths.length - 1] = length(concated[0])
      a.lengths[a.lengths.length - 1] += a.lengths.length > 1 ? a.lengths[a.lengths.length - 2] : 0;

      // Adjust the bottom left side of the new tree.
      if (concated[1].table.length > 0) {
        b.table[0] = concated[1];
        b.lengths[0] = length(concated[1]);
        for (var i = 1, len = length(b.table[0]); i < b.lengths.length; i++) {
          len += length(b.table[i]);
          b.lengths[i] = len;
        }
      } else {
        b.table.shift();
        for (var i = 1; i < b.lengths.length; i++) {
          b.lengths[i] = b.lengths[i] - b.lengths[0];
        }
        b.lengths.shift();
      }

      // Check if balancing is needed and return based on that.
      var toRemove = calcToRemove(a, b);
      if (toRemove <= E || b.table.length == 0) {
        return [a,b];
      }

      return shuffle(a, b, toRemove);
    }

    // Returns the extra search steps for E. Refer to the paper.
    function calcToRemove(a, b) {
      var subLengths = 0;
      for (var i = 0; i < a.table.length; i++) {
        subLengths += a.table[i].table.length;
      }
      for (var i = 0; i < b.table.length; i++) {
        subLengths += b.table[i].table.length;
      }

      var toRemove = a.table.length + b.table.length
      return toRemove - (Math.floor((subLengths - 1) / M) + 1);
    }

    // get2, set2 and saveSlot are helpers for accessing elements over two arrays.
    function get2(a, b, index) {
      return index < a.length ? a[index] : b[index - a.length];
    }

    function set2(a, b, index, value) {
      if (index < a.length) {
        a[index] = value;
      } else {
        b[index - a.length] = value;
      }
    }

    function saveSlot(a, b, index, slot) {
      set2(a.table, b.table, index, slot);

      var l = (index == 0 || index == a.lengths.length) ?
                0 : get2(a.lengths, a.lengths, index - 1);
      set2(a.lengths, b.lengths, index, l + length(slot));
    }

    // Creates a node or leaf with a given length at their arrays for perfomance.
    // Is only used by shuffle.
    function createNode(h, length) {
      if (length < 0) { length = 0; }
      var a = { ctor:"_Array", height:h, table:new Array(length) };
      if (h > 0) {
        a.lengths = new Array(length);
      }
      return a;
    }

    // Returns an array of two balanced nodes.
    function shuffle(a, b, toRemove) {
      var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
      var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

      // Skip the slots with size M. More precise: copy the slot references
      // to the new node
      var read = 0;
      while (get2(a.table, b.table, read).table.length % M == 0) {
        set2(newA.table, newB.table, read, get2(a.table, b.table, read));
        set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
        read++;
      }

      // Pulling items from left to right, caching in a slot before writing
      // it into the new nodes.
      var write = read;
      var slot = new createNode(a.height - 1, 0);
      var from = 0;

      // If the current slot is still containing data, then there will be at
      // least one more write, so we do not break this loop yet.
      while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove) {
        // Find out the max possible items for copying.
        var source = get2(a.table, b.table, read);
        var to = Math.min(M - slot.table.length, source.table.length)

        // Copy and adjust size table.
        slot.table = slot.table.concat(source.table.slice(from, to));
        if (slot.height > 0) {
          var len = slot.lengths.length;
          for (var i = len; i < len + to - from; i++) {
            slot.lengths[i] = length(slot.table[i]);
            slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
          }
        }

        from += to;

        // Only proceed to next slots[i] if the current one was
        // fully copied.
        if (source.table.length <= to) {
          read++; from = 0;
        }

        // Only create a new slot if the current one is filled up.
        if (slot.table.length == M) {
          saveSlot(newA, newB, write, slot);
          slot = createNode(a.height - 1,0);
          write++;
        }
      }

      // Cleanup after the loop. Copy the last slot into the new nodes.
      if (slot.table.length > 0) {
        saveSlot(newA, newB, write, slot);
        write++;
      }

      // Shift the untouched slots to the left
      while (read < a.table.length + b.table.length ) {
        saveSlot(newA, newB, write, get2(a.table, b.table, read));
        read++; write++;
      }

      return [newA, newB];
    }

    // Navigation functions
    function botRight(a) { return a.table[a.table.length - 1]; }
    function botLeft(a)  { return a.table[0]; }

    // Copies a node for updating. Note that you should not use this if
    // only updating only one of "table" or "lengths" for performance reasons.
    function nodeCopy(a) {
      var newA = { ctor:"_Array", height:a.height
                                , table:a.table.slice() };
      if (a.height > 0) { newA.lengths = a.lengths.slice(); }
      return newA;
    }

    // Returns how many items are in the tree.
    function length(a) {
      if (a.height == 0) {
        return a.table.length;
      } else {
        return a.lengths[a.lengths.length - 1];
      }
    }

    // Calculates in which slot of "table" the item probably is, then
    // find the exact slot via forward searching in  "lengths". Returns the index.
    function getSlot(i, a) {
      var slot = Math.floor(i / (Math.pow(M, a.height)));
      while (a.lengths[slot] <= i) {
        slot++
      }
      return slot;
    }

    // Recursively creates a tree with a given height containing
    // only the given item.
    function create(item, h) {
      if (h == 0) {
        return { ctor:"_Array", height:0
                              , table:[item] };
      } else {
        return { ctor:"_Array", height:h
                              , table:[create(item, h - 1)]
                              , lengths:[1] };
      }
    }

    // Recursively creates a tree that contains the given tree.
    function parentise(tree, h) {
      if (h == tree.height) {
        return tree;
      } else {
        return { ctor:"_Array", height:h
                              , table:[parentise(tree, h - 1)]
                              , lengths:[length(tree)] };
      }
    }

    // Emphasizes blood brotherhood beneath two trees.
    function siblise(a, b) {
      return { ctor:"_Array", height:a.height + 1
                            , table:[a, b]
                            , lengths:[length(a), length(a) + length(b)] };
    }

    Elm.Native.Array.values = {
      empty:empty,
      toList:toList,
      concat:F2(concat),
      push:F2(push),
      slice:F3(slice),
      get:F2(get),
      set:F3(set),
      map:F2(map),
      indexedMap:F2(indexedMap),
      foldl:F3(foldl),
      foldr:F3(foldr),
      length:length
    };

    return elm.Native.Array.values = Elm.Native.Array.values;
}
Elm.Native.Show = {};
Elm.Native.Show.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Show = elm.Native.Show || {};
    if (elm.Native.Show.values) return elm.Native.Show.values;

    var NList = Elm.Native.List.make(elm);
    var Array = Elm.Array.make(elm);
    var List = Elm.List.make(elm);
    var Dict = Elm.Dict.make(elm);
    var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

    var toString = function(v) {
        var type = typeof v;
        if (type === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        } else if (type === "boolean") {
            return v ? "True" : "False";
        } else if (type === "number") {
            return v+"";
        } else if ((v instanceof String) && v.isChar) {
            return "'" + addSlashes(v) + "'";
        } else if (type === "string") {
            return '"' + addSlashes(v) + '"';
        } else if (type === "object" && '_' in v && probablyPublic(v)) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i]));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k]));
            }
            if (output.length === 0) return "{}";
            return "{ " + output.join(", ") + " }";
        } else if (type === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k]));
                }
                return "(" + output.join(",") + ")";
            } else if (v.ctor === "_Array") {
                var list = Array.toList(v);
                return "Array.fromList " + toString(list);
            } else if (v.ctor === "::") {
                var output = '[' + toString(v._0);
                v = v._1;
                while (v.ctor === "::") {
                    output += "," + toString(v._0);
                    v = v._1;
                }
                return output + ']';
            } else if (v.ctor === "[]") {
                return "[]";
            } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
                var list = A3(Dict.foldr, cons, NList.Nil, v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list);
            } else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i]);
                    var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : '(' + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (type === 'object' && 'recv' in v) return '<signal>';
        return "<internal structure>";
    };

    function addSlashes(str) {
        return str.replace(/\\/g, '\\\\')
                  .replace(/\n/g, '\\n')
                  .replace(/\t/g, '\\t')
                  .replace(/\r/g, '\\r')
                  .replace(/\v/g, '\\v')
                  .replace(/\0/g, '\\0')
                  .replace(/\'/g, "\\'")
                  .replace(/\"/g, '\\"');
    }

    function probablyPublic(v) {
        var keys = Object.keys(v);
        var len = keys.length;
        if (len === 3
            && 'props' in v
            && 'element' in v) return false;
        if (len === 5
            && 'horizontal' in v
            && 'vertical' in v
            && 'x' in v
            && 'y' in v) return false;
        if (len === 7
            && 'theta' in v
            && 'scale' in v
            && 'x' in v
            && 'y' in v
            && 'alpha' in v
            && 'form' in v) return false;
        return true;
    }

    return elm.Native.Show.values = { show:toString };
};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Debug = elm.Native.Debug || {};
    if (elm.Native.Debug.values) return elm.Native.Debug.values;

    var show = Elm.Native.Show.make(elm).show;

    function log(tag,value) {
        var msg = tag + ': ' + show(value);
        var process = process || {};
        if (process.stdout) {
            process.stdout.write(msg);
        } else {
            console.log(msg);
        }
        return value;
    }

    return elm.Native.Debug.values = {
        log: F2(log)
    };
    
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Transform2D = elm.Native.Transform2D || {};
 if (elm.Native.Transform2D.values) return elm.Native.Transform2D.values;

 var A;
 if (typeof Float32Array === 'undefined') {
     A = function(arr) {
         this.length = arr.length;
         this[0] = arr[0];
         this[1] = arr[1];
         this[2] = arr[2];
         this[3] = arr[3];
         this[4] = arr[4];
         this[5] = arr[5];
     };
 } else {
     A = Float32Array;
 }

 // layout of matrix in an array is
 //
 //   | m11 m12 dx |
 //   | m21 m22 dy |
 //   |  0   0   1 |
 //
 //  new A([ m11, m12, dx, m21, m22, dy ])

 var identity = new A([1,0,0,0,1,0]);
 function matrix(m11, m12, m21, m22, dx, dy) {
     return new A([m11, m12, dx, m21, m22, dy]);
 }
 function rotation(t) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     return new A([c, -s, 0, s, c, 0]);
 }
 function rotate(t,m) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
                   m21*c + m22*s, -m21*s + m22*c, m[5]]);
 }
 /*
 function move(xy,m) {
     var x = xy._0;
     var y = xy._1;
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11, m12, m11*x + m12*y + m[2],
                   m21, m22, m21*x + m22*y + m[5]]);
 }
 function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
 function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
 function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
 function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
 function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

 function transform(m11, m21, m12, m22, mdx, mdy, n) {
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }
 */
 function multiply(m, n) {
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }

 return elm.Native.Transform2D.values = {
     identity:identity,
     matrix:F6(matrix),
     rotation:rotation,
     multiply:F2(multiply)
     /*
     transform:F7(transform),
     rotate:F2(rotate),
     move:F2(move),
     scale:F2(scale),
     scaleX:F2(scaleX),
     scaleY:F2(scaleY),
     reflectX:reflectX,
     reflectY:reflectY
     */
 };

};

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Basics = elm.Native.Basics || {};
  if (elm.Native.Basics.values) return elm.Native.Basics.values;

  var JS = Elm.Native.JavaScript.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function div(a,b) { return (a/b)|0; }
  function rem(a,b) { return a % b; }
  var mod = Utils.mod;
  function abs(x) { return x < 0 ? -x : x; }
  function logBase(base,n) { return Math.log(n) / Math.log(base); }
  function min(a,b) { return Utils.cmp(a,b) < 0 ? a : b; }
  function max(a,b) { return Utils.cmp(a,b) > 0 ? a : b; }
  function clamp(lo,hi,n) {
      return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n; }
  function xor(a,b) { return a !== b; }
  function not(b) { return !b; }
  function isInfinite(n) { return n === Infinity || n === -Infinity }

  function truncate(n) { return n|0; }

  var basics = {
      div:F2(div),
      rem:F2(rem),
      mod:mod,

      pi:Math.PI,
      e:Math.E,
      cos:Math.cos,
      sin:Math.sin,
      tan:Math.tan,
      acos:Math.acos,
      asin:Math.asin,
      atan:Math.atan,
      atan2:F2(Math.atan2),

      sqrt:Math.sqrt,
      abs:abs,
      logBase:F2(logBase),
      min:F2(min),
      max:F2(max),
      clamp:F3(clamp),
      compare:Utils.compare,

      xor:F2(xor),
      not:not,

      truncate:truncate,
      ceiling:Math.ceil,
      floor:Math.floor,
      round:Math.round,
      toFloat:function(x) { return x; },
      isNaN:isNaN,
      isInfinite:isInfinite
  };

  return elm.Native.Basics.values = basics;
};
Elm.Native.Text = {};
Elm.Native.Text.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Text = elm.Native.Text || {};
    if (elm.Native.Text.values) return elm.Native.Text.values;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Element = Elm.Graphics.Element.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function makeSpaces(s) {
        if (s.length == 0) { return s; }
        var arr = s.split('');
        if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
        for (var i = arr.length; --i; ) {
            if (arr[i][0] == ' ' && arr[i-1] == ' ') {
                arr[i-1] = arr[i-1] + arr[i];
                arr[i] = '';
            }
        }
        for (var i = arr.length; i--; ) {
            if (arr[i].length > 1 && arr[i][0] == ' ') {
                var spaces = arr[i].split('');
                for (var j = spaces.length - 2; j >= 0; j -= 2) {
                    spaces[j] = '&nbsp;';
                }
                arr[i] = spaces.join('');
            }
        }
        arr = arr.join('');
        if (arr[arr.length-1] === " ") {
	    return arr.slice(0,-1) + '&nbsp;';
        }
        return arr;
    }

    function properEscape(str) {
        if (str.length == 0) return str;
        str = str //.replace(/&/g,  "&#38;")
	    .replace(/"/g,  '&#34;')
	    .replace(/'/g,  "&#39;")
	    .replace(/</g,  "&#60;")
	    .replace(/>/g,  "&#62;")
	    .replace(/\n/g, "<br/>");
        var arr = str.split('<br/>');
        for (var i = arr.length; i--; ) {
	    arr[i] = makeSpaces(arr[i]);
        }
        return arr.join('<br/>');
    }

    function toText(str) { return Utils.txt(properEscape(str)); }

    // conversions from Elm values to CSS
    function toTypefaces(list) {
        var typefaces = List.toArray(list);
        for (var i = typefaces.length; i--; ) {
            var typeface = typefaces[i];
            if (typeface.indexOf(' ') > -1) {
                typefaces[i] = "'" + typeface + "'";
            }
        }
        return typefaces.join(',');
    }
    function toLine(line) {
        var ctor = line.ctor;
        return ctor === 'Under' ? 'underline' :
               ctor === 'Over'  ? 'overline'  : 'line-through';
    }

    // setting styles of Text
    function style(style, text) {
        var newText = '<span style="color:' + toCss(style.color) + ';'
        if (style.typeface.ctor !== '[]') {
            newText += 'font-family:' + toTypefaces(style.typeface) + ';'
        }
        if (style.height.ctor !== "Nothing") {
            newText += 'font-size:' + style.height._0 + 'px;';
        }
        if (style.bold) {
            newText += 'font-weight:bold;';
        }
        if (style.italic) {
            newText += 'font-style:italic;';
        }
        if (style.line.ctor !== 'Nothing') {
            newText += 'text-decoration:' + toLine(style.line._0) + ';';
        }
        newText += '">' + Utils.makeText(text) + '</span>'
        return Utils.txt(newText);
    }
    function height(px, text) {
        return { style: 'font-size:' + px + 'px;', text:text }
    }
    function typeface(names, text) {
        return { style: 'font-family:' + toTypefaces(names) + ';', text:text }
    }
    function monospace(text) {
        return { style: 'font-family:monospace;', text:text }
    }
    function italic(text) {
        return { style: 'font-style:italic;', text:text }
    }
    function bold(text) {
        return { style: 'font-weight:bold;', text:text }
    }
    function link(href, text) {
        return { href: toText(href), text:text };
    }
    function line(line, text) {
        return { style: 'text-decoration:' + toLine(line) + ';', text:text };
    }

    function color(color, text) {
        return { style: 'color:' + toCss(color) + ';', text:text };
    }

    function block(align) {
        return function(text) {
            var raw = {
                ctor :'RawHtml',
                html : Utils.makeText(text),
                align: align,
                guid : null,
                args : [],
            };
            var pos = A2(Utils.htmlHeight, 0, raw);
            return A3(Element.newElement, pos._0, pos._1, raw);
        }
    }

    function markdown(text, guid) {
        var raw = {
            ctor:'RawHtml',
            html: text,
            align: null,
            guid: guid,
            args: [],
        };
        var pos = A2(Utils.htmlHeight, 0, raw);
        return A3(Element.newElement, pos._0, pos._1, raw);
    }

    return elm.Native.Text.values = {
        toText: toText,

        height : F2(height),
        italic : italic,
        bold : bold,
        line : F2(line),
        monospace : monospace,
        typeface : F2(typeface),
        color : F2(color),
        link : F2(link),

        leftAligned  : block('left'),
        rightAligned : block('right'),
        centered     : block('center'),
        justified    : block('justify'),
        markdown     : markdown,

        toTypefaces:toTypefaces,
        toLine:toLine,
    };
};
Elm.Native.Trampoline = {};
Elm.Native.Trampoline.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Trampoline = elm.Native.Trampoline || {};
    if (elm.Native.Trampoline.values) return elm.Native.Trampoline.values;

    // trampoline : Trampoline a -> a
    function trampoline(t) {
        var tramp = t;
        while(true) {
            switch(tramp.ctor) {
            case "Done":
                return tramp._0;
            case "Continue":
                tramp = tramp._0({ctor: "_Tuple0"});
                continue;
            }
        }
    }

    return elm.Native.Trampoline.values = {
        trampoline:trampoline
    };
};
Elm.Native.Ports = {};
Elm.Native.Ports.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Ports = elm.Native.Ports || {};
    if (elm.Native.Ports.values) return elm.Native.Ports.values;

    var Signal = Elm.Signal.make(elm);

    function incomingSignal(converter) {
        converter.isSignal = true;
        return converter;
    }

    function outgoingSignal(converter) {
        return function(signal) {
            var subscribers = []
            function subscribe(handler) {
                subscribers.push(handler);
            }
            function unsubscribe(handler) {
                subscribers.pop(subscribers.indexOf(handler));
            }
            A2( Signal.lift, function(value) {
                var val = converter(value);
                var len = subscribers.length;
                for (var i = 0; i < len; ++i) {
                    subscribers[i](val);
                }
            }, signal);
            return { subscribe:subscribe, unsubscribe:unsubscribe };
        }
    }

    function portIn(name, converter) {
        var jsValue = elm.ports.incoming[name];
        if (jsValue === undefined) {
            throw new Error("Initialization Error: port '" + name +
                            "' was not given an input!");
        }
        elm.ports.uses[name] += 1;
        try {
            var elmValue = converter(jsValue);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }

        // just return a static value if it is not a signal
        if (!converter.isSignal) {
            return elmValue;
        }

        // create a signal if necessary
        var signal = Signal.constant(elmValue);
        function send(jsValue) {
            try {
                var elmValue = converter(jsValue);
            } catch(e) {
                throw new Error("Error sending to port '" + name + "': \n" + e.message);
            }
            setTimeout(function() {
                elm.notify(signal.id, elmValue);
            }, 0);
        }
        elm.ports.outgoing[name] = { send:send };
        return signal;
    }

    function portOut(name, converter, value) {
        try {
            elm.ports.outgoing[name] = converter(value);
        } catch(e) {
            throw new Error("Initialization Error on port '" + name + "': \n" + e.message);
        }
        return value;
    }

    return elm.Native.Ports.values = {
        incomingSignal: incomingSignal,
        outgoingSignal: outgoingSignal,
        portOut: portOut,
        portIn: portIn
    };
};
Elm.Native.Color = {};
Elm.Native.Color.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Color = elm.Native.Color || {};
    if (elm.Native.Color.values) return elm.Native.Color.values;

    var Utils = Elm.Native.Utils.make(elm);

    function toCss(c) {
        return (c._3 === 1)
            ? ('rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')')
            : ('rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')');
    }

    function complement(rgb) {
        var hsv = toHSV(rgb);
        hsv.hue = (hsv.hue + 180) % 360;
        return toRGB(hsv);
    }

    function hsva(h,s,v,a) {
        var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
        var clr = toRGB({hue:degree, saturation:s, value:v});
        clr._3 = a;
        return clr;
    }

    function hsv(h,s,v) {
        var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
        return toRGB({hue:degree, saturation:s, value:v});
    }

    function toHSV(rgb) {
        var hsv = {};
        var r = rgb._0 / 255.0, g = rgb._1 / 255.0, b = rgb._2 / 255.0;
        var M = Math.max(r,g,b);
        var m = Math.min(r,g,b);
        var c = M - m;

        var h = 0;
             if (c === 0) { h = 0; }
        else if (M === r) { h = ((g - b) / c) % 6; }
        else if (M === g) { h = ((b - r) / c) + 2; }
        else if (M === b) { h = ((r - g) / c) + 4; }
        h *= 60;

        return { value : M, hue : h, saturation : (M === 0 ? 0 : c / M) };
    }

    function between(lo,hi,x) { return lo <= x && x < hi; }
    function norm(n) { return Math.round(n*255); }

    function toRGB(hsv) {
        var c = hsv.value * hsv.saturation;
        var hue = hsv.hue / 60;
        var x = c * (1 - Math.abs((hue % 2) - 1));
        var r = 0, g = 0, b = 0;
             if (between(0,1,hue)) { r = c; g = x; b = 0; }
        else if (between(1,2,hue)) { r = x; g = c; b = 0; }
        else if (between(2,3,hue)) { r = 0; g = c; b = x; }
        else if (between(3,4,hue)) { r = 0; g = x; b = c; }
        else if (between(4,5,hue)) { r = x; g = 0; b = c; }
        else if (between(5,6,hue)) { r = c; g = 0; b = x; }

        var m = hsv.value - c;
        return { ctor:"Color", _0:norm(r+m), _1:norm(g+m), _2:norm(b+m), _3:1 };
    }

    return elm.Native.Color.values = {
        hsva:F4(hsva),
        hsv:F3(hsv),
        complement:complement,
        toCss:toCss
    };

};
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
Elm.Native.Error = {};
Elm.Native.Error.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Error = elm.Native.Error || {};
    if (elm.Native.Error.values) return elm.Native.Error.values;

    var fromString = Elm.Native.JavaScript.make(elm).fromString;

    function indent(lines) {
        var msg = '';
        for (var i = 0; i < lines.length; ++i) {
            msg += '<br/>&nbsp; &nbsp; ' + lines[i];
        }
        return msg;
    }

    function Case(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in case-expression.',
                          'Make sure your patterns cover every case!']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function If(moduleName, span) { 
	var msg = indent(['Non-exhaustive pattern match in multi-way-if expression.',
                          'It is best to use \'otherwise\' as the last branch of multi-way-if.']);
	throw new Error('Runtime error in module ' + moduleName + ' (' + span + '):' + msg);
    }

    function raise(str) { throw new Error(fromString(str)); }

    return elm.Native.Error.values = { Case: Case, If: If, raise: raise };
};Elm.Native.List = {};
Elm.Native.List.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.List = elm.Native.List || {};
    if (elm.Native.List.values) return elm.Native.List.values;
    if ('values' in Elm.Native.List)
        return elm.Native.List.values = Elm.Native.List.values;

    var Utils = Elm.Native.Utils.make(elm);

    // TODO: Improve Nil handling
    // We can change places like:  if (xs.ctor === '[]') ... to if (xs === Nil) ...
    // but only if we're confident Nil can only be defined once.
    // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
    // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
    // So, this can only be fixed when modules initialisation is also fixed.
    // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
    // (on firefox 19)

    var Nil = { ctor:'[]' };

    // using freeze for every cons would be nice but is a huge (9x on firefox 19)
    // performance penalty
    function Cons(hd,tl) { return { ctor:"::", _0:hd, _1:tl }; }

    function throwError(f) {
        throw new Error("Function '" + f + "' expects a non-empty list!");
    }

    function toArray(xs) {
        var out = [];
        while (xs.ctor !== '[]') {
            out.push(xs._0);
            xs = xs._1;
        }
        return out;
    }

    function fromArray(arr) {
        var out = Nil;
        for (var i = arr.length; i--; ) {
            out = Cons(arr[i], out);
        }
        return out;
    }

    function range(lo,hi) {
        var lst = Nil;
        if (lo <= hi) {
            do { lst = Cons(hi,lst) } while (hi-->lo);
        }
        return lst
    }

    function append(xs,ys) {
        // append Text
        if (xs.text || ys.text) {
            return Utils.txt(Utils.makeText(xs) + Utils.makeText(ys));
        }

        // append Strings
        if (typeof xs === "string") return xs + ys;

        // append Lists
        if (xs.ctor === '[]') { return ys; }
        var root = Cons(xs._0, Nil);
        var curr = root;
        xs = xs._1;
        while (xs.ctor !== '[]') {
	    curr._1 = Cons(xs._0, Nil);
	    xs = xs._1;
	    curr = curr._1;
        }
        curr._1 = ys;
        return root;
    }

    function head(v) { return v.ctor === '[]' ? throwError('head') : v._0; }
    function tail(v) { return v.ctor === '[]' ? throwError('tail') : v._1; }

    function last(xs) {
        if (xs.ctor === '[]') { throwError('last'); }
        var out = xs._0;
        while (xs.ctor !== '[]') {
            out = xs._0;
            xs = xs._1;
        }
        return out;
    }

    function map(f, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            arr.push(f(xs._0));
            xs = xs._1;
        }
        return fromArray(arr);
    }

    // f defined similarly for both foldl and foldr (NB: different from Haskell)
    // ie, foldl : (a -> b -> b) -> b -> [a] -> b
    function foldl(f, b, xs) {
        var acc = b;
        while (xs.ctor !== '[]') {
            acc = A2(f, xs._0, acc);
            xs = xs._1;
        }
        return acc;
    }

    function foldr(f, b, xs) {
        var arr = toArray(xs);
        var acc = b;
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function foldl1(f, xs) {
        return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
    }

    function foldr1(f, xs) {
        if (xs.ctor === '[]') { throwError('foldr1'); }
        var arr = toArray(xs);
        var acc = arr.pop();
        for (var i = arr.length; i--; ) {
            acc = A2(f, arr[i], acc);
        }
        return acc;
    }

    function scanl(f, b, xs) {
        var arr = toArray(xs);
        arr.unshift(b);
        var len = arr.length;
        for (var i = 1; i < len; ++i) {
            arr[i] = A2(f, arr[i], arr[i-1]);
        }
        return fromArray(arr);
    }

    function scanl1(f, xs) {
        return xs.ctor === '[]' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
    }

    function filter(pred, xs) {
        var arr = [];
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) { arr.push(xs._0); }
            xs = xs._1;
        }
        return fromArray(arr);
    }

    function length(xs) {
        var out = 0;
        while (xs.ctor !== '[]') {
            out += 1;
            xs = xs._1;
        }
        return out;
    }

    function member(x, xs) {
        while (xs.ctor !== '[]') {
            if (Utils.eq(x,xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function reverse(xs) { return fromArray(toArray(xs).reverse()); }

    function concat(xss) {
        if (xss.ctor === '[]') return xss;
        var arr = toArray(xss);
        var xs = arr[arr.length-1];
        for (var i = arr.length-1; i--; ) {
	    xs = append(arr[i], xs);
        }
        return xs;
    }

    function all(pred, xs) {
        while (xs.ctor !== '[]') {
            if (!pred(xs._0)) return false;
            xs = xs._1;
        }
        return true;
    }

    function any(pred, xs) {
        while (xs.ctor !== '[]') {
            if (pred(xs._0)) return true;
            xs = xs._1;
        }
        return false;
    }

    function zipWith(f, xs, ys) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]') {
            arr.push(A2(f, xs._0, ys._0));
            xs = xs._1;
            ys = ys._1;
        }
        return fromArray(arr);
    }

    function zip(xs, ys) {
        var arr = [];
        while (xs.ctor !== '[]' && ys.ctor !== '[]') {
            arr.push(Utils.Tuple2(xs._0, ys._0));
            xs = xs._1;
            ys = ys._1;
        }
        return fromArray(arr);
    }

    function sort(xs) {
        return fromArray(toArray(xs).sort(Utils.cmp));
    }

    function sortBy(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            return Utils.cmp(f(a), f(b));
        }));
    }

    function sortWith(f, xs) {
        return fromArray(toArray(xs).sort(function(a,b){
            var ord = f(a)(b).ctor;
            return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
        }));
    }

    function nth(xs, n) {
        return toArray(xs)[n];
    }

    function take(n, xs) {
        var arr = [];
        while (xs.ctor !== '[]' && n > 0) {
            arr.push(xs._0);
            xs = xs._1;
            --n;
        }
        return fromArray(arr);
    }

    function drop(n, xs) {
        while (xs.ctor !== '[]' && n > 0) {
            xs = xs._1;
            --n;
        }
        return xs;
    }

    function repeat(n, x) {
        var arr = [];
        var pattern = [x];
        while (n > 0) {
            if (n & 1) arr = arr.concat(pattern);
            n >>= 1, pattern = pattern.concat(pattern);
        }
        return fromArray(arr);
    }

    function join(sep, xss) {
        if (sep.text) {
            sep = Utils.makeText(sep);
            xss = toArray(xss);
            for (var i = xss.length; i--; ) {
                xss[i] = Utils.makeText(xss[i]);
            }
            return Utils.txt(xss.join(sep));
        }
        if (typeof sep === 'string') return toArray(xss).join(sep);
        if (xss.ctor === '[]') return Nil;
        var s = toArray(sep);
        var out = toArray(xss._0);
        xss = xss._1;
        while (xss.ctor !== '[]') {
            out = out.concat(s, toArray(xss._0));
            xss = xss._1;
        }
        return fromArray(out);
    }

    Elm.Native.List.values = {
        Nil:Nil,
        Cons:Cons,
        cons:F2(Cons),
        toArray:toArray,
        fromArray:fromArray,
        range:range,
        append:append,

        head:head,
        tail:tail,
        last:last,

        map:F2(map),
        foldl:F3(foldl),
        foldr:F3(foldr),

        foldl1:F2(foldl1),
        foldr1:F2(foldr1),
        scanl:F3(scanl),
        scanl1:F2(scanl1),
        filter:F2(filter),
        length:length,
        member:F2(member),
        reverse:reverse,
        concat:concat,

        all:F2(all),
        any:F2(any),
        zipWith:F3(zipWith),
        zip:F2(zip),
        sort:sort,
        sortBy:F2(sortBy),
        sortWith:F2(sortWith),
        nth:F2(nth),
        take:F2(take),
        drop:F2(drop),
        repeat:F2(repeat),

        join:F2(join)
    };
    return elm.Native.List.values = Elm.Native.List.values;

};
Elm.Native.Json = {};
Elm.Native.Json.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Json = elm.Native.Json || {};
    if (elm.Native.Json.values) return elm.Native.Json.values;

    var Maybe = Elm.Maybe.make(elm);
    var Dict = Elm.Dict.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function toJS(v) {
        switch (v.ctor) {
        case 'Null'   : return null;
        case 'String' : return v._0;
        case 'Number' : return v._0;
        case 'Boolean': return v._0;
        case 'Object' :
            var obj = {};
            var array = List.toArray(Dict.toList(v._0));
            for (var i = array.length; i--; ) {
                var entry = array[i];
                obj[entry._0] = toJS(entry._1);
            }
            return obj;
        case 'Array'  :
            var array = List.toArray(v._0);
            for (var i = array.length; i--; ) {
	        array[i] = toJS(array[i]);
            }
            return array;
        }
    }

    function toString(sep, value) {
        return JSON.stringify(toJS(value), null, sep);
    }

    function fromJS(v) {
        switch (typeof v) {
        case 'string' : return { ctor:"String" , _0: v };
        case 'number' : return { ctor:"Number" , _0: v };
        case 'boolean': return { ctor:"Boolean", _0: v };
        case 'object' :
            if (v === null) return { ctor:"Null" };
            if (v instanceof Array) {
                var array = new Array(v.length);
                for (var i = v.length; i--; ) {
                    array[i] = fromJS(v[i]);
                }
	        return {
                    ctor:"Array",
                    _0: List.fromArray(array)
                };
            }
            var array = [];
            for (var key in v) {
                var value = fromJS(v[key]);
                array.push(Utils.Tuple2(key, value));
            }
            var list = List.fromArray(array);
            return {
                ctor:"Object",
                _0: Dict.fromList(list)
            };
        }
    }

    function fromString(str) {
        try {
	    return Maybe.Just(fromJS(JSON.parse(str)));
        } catch (e) {
	    return Maybe.Nothing;
        }
    }

    return elm.Native.Json.values = {
        toString   : F2(toString),
        fromString : fromString,
        fromJS     : fromJS,
        toJS       : toJS,
    };

};
Elm.Native.String = {};
Elm.Native.String.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.String = elm.Native.String || {};
    if (elm.Native.String.values) return elm.Native.String.values;
    if ('values' in Elm.Native.String)
        return elm.Native.String.values = Elm.Native.String.values;

    var Char = Elm.Char.make(elm);
    var Maybe = Elm.Maybe.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function isEmpty(str) {
        return str.length === 0;
    }
    function cons(chr,str) {
        return chr + str;
    }
    function uncons(str) {
        var hd;
        return (hd = str[0]) ? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
                              : Maybe.Nothing;
    }
    function append(a,b) {
        return a + b;
    }
    function concat(strs) {
        return List.toArray(strs).join('');
    }
    function length(str) {
        return str.length;
    }
    function map(f,str) {
        var out = str.split('');
        for (var i = out.length; i--; ) {
            out[i] = f(Utils.chr(out[i]));
        }
        return out.join('');
    }
    function filter(pred,str) {
        return str.split('').map(Utils.chr).filter(pred).join('');
    }
    function reverse(str) {
        return str.split('').reverse().join('');
    }
    function foldl(f,b,str) {
        var len = str.length;
        for (var i = 0; i < len; ++i) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }
    function foldr(f,b,str) {
        for (var i = str.length; i--; ) {
            b = A2(f, Utils.chr(str[i]), b);
        }
        return b;
    }

    function split(sep, str) {
        return List.fromArray(str.split(sep));
    }
    function join(sep, strs) {
        return List.toArray(strs).join(sep);
    }
    function repeat(n, str) {
        var result = '';
        while (n > 0) {
            if (n & 1) result += str;
            n >>= 1, str += str;
        }
        return result;
    }

    function sub(start, end, str) {
        return str.slice(start,end);
    }
    function left(n, str) {
        return n < 1 ? "" : str.slice(0,n);
    }
    function right(n, str) {
        return n < 1 ? "" : str.slice(-n);
    }
    function dropLeft(n, str) {
        return n < 1 ? str : str.slice(n);
    }
    function dropRight(n, str) {
        return n < 1 ? str : str.slice(0,-n);
    }

    function pad(n,chr,str) {
        var half = (n - str.length) / 2;
        return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
    }
    function padRight(n,chr,str) {
        return str + repeat(n - str.length, chr);
    }
    function padLeft(n,chr,str) {
        return repeat(n - str.length, chr) + str;
    }

    function trim(str) {
        return str.trim();
    }
    function trimLeft(str) {
        return str.trimLeft();
    }
    function trimRight(str) {
        return str.trimRight();
    }

    function words(str) {
        return List.fromArray(str.split(/\s+/g));
    }
    function lines(str) {
        return List.fromArray(str.split(/\r\n|\r|\n/g));
    }

    function toUpper(str) {
        return str.toUpperCase();
    }
    function toLower(str) {
        return str.toLowerCase();
    }

    function any(pred, str) {
        for (var i = str.length; i--; ) {
            if (pred(Utils.chr(str[i]))) return true;
        }
        return false;
    }
    function all(pred, str) {
        for (var i = str.length; i--; ) {
            if (!pred(Utils.chr(str[i]))) return false;
        }
        return true;
    }

    function contains(sub, str) {
        return str.indexOf(sub) > -1;
    }
    function startsWith(sub, str) {
        return str.indexOf(sub) === 0;
    }
    function endsWith(sub, str) {
        return str.length >= sub.length &&
               str.lastIndexOf(sub) === str.length - sub.length;
    }
    function indexes(sub, str) {
        var subLen = sub.length;
        var i = 0;
        var is = [];
        while ((i = str.indexOf(sub, i)) > -1) {
            is.push(i);
            i = i + subLen;
        }
        return List.fromArray(is);
    }

    function toInt(s) {
        var len = s.length;
        if (len === 0) { return Maybe.Nothing; }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) { return Maybe.Nothing; }
            start = 1;
        }
        for (var i = start; i < len; ++i) {
            if (!Char.isDigit(s[i])) { return Maybe.Nothing; }
        }
        return Maybe.Just(parseInt(s, 10));
    }

    function toFloat(s) {
        var len = s.length;
        if (len === 0) { return Maybe.Nothing; }
        var start = 0;
        if (s[0] == '-') {
            if (len === 1) { return Maybe.Nothing; }
            start = 1;
        }
        var dotCount = 0;
        for (var i = start; i < len; ++i) {
            if (Char.isDigit(s[i])) { continue; }
            if (s[i] === '.') {
                dotCount += 1;
                if (dotCount <= 1) { continue; }
            }
            return Maybe.Nothing;
        }
        return Maybe.Just(parseFloat(s));
    }

    function toList(str) {
        return List.fromArray(str.split('').map(Utils.chr));
    }
    function fromList(chars) {
        return List.toArray(chars).join('');
    }

    return Elm.Native.String.values = {
        isEmpty: isEmpty,
        cons: F2(cons),
        uncons: uncons,
        append: F2(append),
        concat: concat,
        length: length,
        map: F2(map),
        filter: F2(filter),
        reverse: reverse,
        foldl: F3(foldl),
        foldr: F3(foldr),

        split: F2(split),
        join: F2(join),
        repeat: F2(repeat),

        sub: F3(sub),
        left: F2(left),
        right: F2(right),
        dropLeft: F2(dropLeft),
        dropRight: F2(dropRight),

        pad: F3(pad),
        padLeft: F3(padLeft),
        padRight: F3(padRight),

        trim: trim,
        trimLeft: trimLeft,
        trimRight: trimRight,

        words: words,
        lines: lines,

        toUpper: toUpper,
        toLower: toLower,

        any: F2(any),
        all: F2(all),

        contains: F2(contains),
        startsWith: F2(startsWith),
        endsWith: F2(endsWith),
        indexes: F2(indexes),

        toInt: toInt,
        toFloat: toFloat,
        toList: toList,
        fromList: fromList,
    };
};
Elm.Native.Char = {};
Elm.Native.Char.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Char = elm.Native.Char || {};
    if (elm.Native.Char.values) return elm.Native.Char.values;

    function isBetween(lo,hi) { return function(chr) {
	var c = chr.charCodeAt(0);
	return lo <= c && c <= hi;
    };
                              }
    var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
    var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
    var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));

    return elm.Native.Char.values = {
        fromCode : function(c) { return String.fromCharCode(c); },
        toCode   : function(c) { return c.toUpperCase().charCodeAt(0); },
        toUpper  : function(c) { return c.toUpperCase(); },
        toLower  : function(c) { return c.toLowerCase(); },
        toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
        toLocaleLower : function(c) { return c.toLocaleLowerCase(); },
        isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
        isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
        isDigit    : isDigit,
        isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
        isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
    };
};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Utils = elm.Native.Utils || {};
    if (elm.Native.Utils.values) return elm.Native.Utils.values;

    function eq(x,y) {
        if (x === y) return true;
        if (typeof x === "object") {
            var c = 0;
            for (var i in x) {
                ++c;
                if (!eq(x[i],y[i])) {
                    return false;
                }
            }
            return c === Object.keys(y).length;
        }
        if (typeof x === 'function') {
            throw new Error('Equality error: general function equality is ' +
                            'undecidable, and therefore, unsupported');
        }
        return x === y;
    }

    // code in Generate/JavaScript.hs depends on the particular
    // integer values assigned to LT, EQ, and GT
    var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];
    function compare(x,y) { return { ctor: ord[cmp(x,y)+1] } }
    function cmp(x,y) {
        var ord;
        if (typeof x !== 'object' || x instanceof String){
            return x === y ? EQ : x < y ? LT : GT;
        }

        if (x.ctor === "::" || x.ctor === "[]") {
            while (true) {
                if (x.ctor === "[]" && y.ctor === "[]") return EQ;
                if (x.ctor !== y.ctor) return x.ctor === '[]' ? LT : GT;
                ord = cmp(x._0, y._0);
                if (ord !== EQ) return ord;
                x = x._1;
                y = y._1;
            }
        }

        if (x.ctor.slice(0,6) === '_Tuple') {
            var n = x.ctor.slice(6) - 0;
            var err = 'cannot compare tuples with more than 6 elements.';
            if (n === 0) return EQ;
            if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
            if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
            if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
            if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
            if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
            if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
            if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
            return EQ;
        }
        throw new Error('Comparison error: comparison is only defined on ints, ' +
                        'floats, times, chars, strings, lists of comparable values, ' +
                        'and tuples of comparable values.')
    }


    var Tuple0 = { ctor: "_Tuple0" };
    function Tuple2(x,y) { return { ctor:"_Tuple2", _0:x, _1:y } }

    function chr(c) {
        var x = new String(c);
        x.isChar = true;
        return x;
    }

    function txt(str) {
        var t = new String(str);
        t.text = true;
        return t;
    }

    function makeText(text) {
        var style = '';
        var href = '';
        while (true) {
            if (text.style) {
                style += text.style;
                text = text.text;
                continue;
            }
            if (text.href) {
                href = text.href;
                text = text.text;
                continue;
            }
            if (href) text = '<a href="' + href + '">' + text + '</a>';
            if (style) text = '<span style="' + style + '">' + text + '</span>';
            return text;
        }
    }

    var count = 0;
    function guid(_) { return count++ }

    function copy(r) {
        var o = {};
        for (var i in r) { o[i] = r[i]; }
        return o;
    }

    function remove(x,r) {
        var o = copy(r);
        if (x in o._) {
            o[x] = o._[x][0];
            o._[x] = o._[x].slice(1);
            if (o._[x].length === 0) { delete o._[x]; }
        } else {
            delete o[x];
        }
        return o;
    }

    function replace(kvs,r) {
        var o = copy(r);
        for (var i = kvs.length; i--; ) {
            var kvsi = kvs[i];
            o[kvsi[0]] = kvsi[1];
        }
        return o;
    }

    function insert(x,v,r) {
        var o = copy(r);
        if (x in o) o._[x] = [o[x]].concat(x in o._ ? o._[x].slice(0) : []);
        o[x] = v;
        return o;
    }

    function max(a,b) { return a > b ? a : b }
    function min(a,b) { return a < b ? a : b }

    function mod(a,b) {
        if (b === 0) {
            throw new Error("Cannot perform mod 0. Division by zero error.");
        }
        var r = a % b;
        var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

        return m === b ? 0 : m;
    }

    function htmlHeight(width, rawHtml) {
        // create dummy node
        var html = rawHtml.html;
        var t = document.createElement('div');
        t.innerHTML = html;
        if (width > 0) { t.style.width = width + "px"; }
        t.style.visibility = "hidden";
        t.style.styleFloat = "left";
        t.style.cssFloat   = "left";

        document.body.appendChild(t);

        // insert interpolated values
        var args = rawHtml.args;
        var guid = rawHtml.guid;
        for (var i = args.length; i--; ) {
            var arg = args[i];
            var span = document.getElementById('md-' + guid + '-' + i);
            if (arg.isElement) {
                span.style.width = arg.props.width + 'px';
                span.style.height = arg.props.height + 'px';
            } else {
                span.innerHTML = arg;
            }
        }

        // get dimensions
        var style = window.getComputedStyle(t, null);
        var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
        var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
        document.body.removeChild(t);
        return Tuple2(w,h);
    }

    function getXY(e) {
        var posx = 0;
        var posy = 0;
        if (e.pageX || e.pageY) {
            posx = e.pageX;
            posy = e.pageY;
        } else if (e.clientX || e.clientY) {
            posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
            posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
        }

        if (elm.display === ElmRuntime.Display.COMPONENT) {
            var rect = elm.node.getBoundingClientRect();
            var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
            var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
            // TODO: figure out if there is a way to avoid rounding here
            posx = posx - Math.round(relx) - elm.node.clientLeft;
            posy = posy - Math.round(rely) - elm.node.clientTop;
        }
        return Tuple2(posx, posy);
    }

    return elm.Native.Utils.values = {
        eq:eq,
        cmp:cmp,
        compare:F2(compare),
        Tuple0:Tuple0,
        Tuple2:Tuple2,
        chr:chr,
        txt:txt,
        makeText:makeText,
        copy: copy,
        remove: remove,
        replace: replace,
        insert: insert,
        guid: guid,
        max : F2(max),
        min : F2(min),
        mod : F2(mod),
        htmlHeight: F2(htmlHeight),
        getXY: getXY,
        toFloat: function(x) { return +x; }
    };
};
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
        shiftRightLogical   : F2(srl),
    };
    
};

function F2(fun) {
  function wrapper(a) { return function(b) { return fun(a,b) } }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun) {
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a,b,c) }}
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a,b,c,d) }}}
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return fun(a,b,c,d,e,f) }}}}}
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) {return fun(a,b,c,d,e,f,g,h)}}}}}}}
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) { return function(i) {
        return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun,a,b) {
  return fun.arity === 2 ? fun.func(a,b) : fun(a)(b);
}
function A3(fun,a,b,c) {
  return fun.arity === 3 ? fun.func(a,b,c) : fun(a)(b)(c);
}
function A4(fun,a,b,c,d) {
  return fun.arity === 4 ? fun.func(a,b,c,d) : fun(a)(b)(c)(d);
}
function A5(fun,a,b,c,d,e) {
  return fun.arity === 5 ? fun.func(a,b,c,d,e) : fun(a)(b)(c)(d)(e);
}
function A6(fun,a,b,c,d,e,f) {
  return fun.arity === 6 ? fun.func(a,b,c,d,e,f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun,a,b,c,d,e,f,g) {
  return fun.arity === 7 ? fun.func(a,b,c,d,e,f,g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun,a,b,c,d,e,f,g,h) {
  return fun.arity === 8 ? fun.func(a,b,c,d,e,f,g,h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun,a,b,c,d,e,f,g,h,i) {
  return fun.arity === 9 ? fun.func(a,b,c,d,e,f,g,h,i)
                         : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
Elm.Native.Graphics.Input = {};
Elm.Native.Graphics.Input.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.Input = elm.Native.Graphics.Input || {};
    if (elm.Native.Graphics.Input.values) return elm.Native.Graphics.Input.values;

    var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    var toCss = Elm.Native.Color.make(elm).toCss;
    var Text = Elm.Native.Text.make(elm);
    var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Tuple2 = Utils.Tuple2;

    function input(initialValue) {
        var signal = Signal.constant(initialValue);
        return { _:{}, signal:signal, handle:signal };
    }

    function renderDropDown(signal, values) {
        return function(_) {
            var entries = List.toArray(values);

            var drop = newNode('select');
            drop.style.border = '0 solid';
            drop.style.pointerEvents = 'auto';
            for (var i = 0; i < entries.length; ++i) {
                var option = newNode('option');
                var name = entries[i]._0;
                option.value = name;
                option.innerHTML = name;
                drop.appendChild(option);
            }
            drop.addEventListener('change', function() {
                elm.notify(signal.id, entries[drop.selectedIndex]._1);
            });

            var t = drop.cloneNode(true);
            t.style.visibility = "hidden";

            elm.node.appendChild(t);
            var style = window.getComputedStyle(t, null);
            var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
            var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
            elm.node.removeChild(t);
            return drop;
        };
    }

    function updateDropDown(node, oldModel, newModel) {
    }

    function dropDown(signal, values) {
        return A3(newElement, 100, 24, {
            ctor: 'Custom',
            type: 'DropDown',
            render: renderDropDown(signal,values),
            update: updateDropDown,
            model: {}
        });
    }

    function renderButton(model) {
        var node = newNode('button');
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_value = model.value;
        function click() {
            elm.notify(node.elm_signal.id, node.elm_value);
        }
        node.addEventListener('click', click);
        node.innerHTML = model.text;
        return node;
    }

    function updateButton(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_value = newModel.value;
        var txt = newModel.text;
        if (oldModel.text !== txt) node.innerHTML = txt;
    }

    function button(signal, value, text) {
        return A3(newElement, 100, 40, {
            ctor: 'Custom',
            type: 'Button',
            render: renderButton,
            update: updateButton,
            model: { signal:signal, value:value, text:text }
        });
    }

    function renderCustomButton(model) {
        var btn = newNode('div');
        btn.style.pointerEvents = 'auto';
        btn.elm_signal = model.signal;
        btn.elm_value = model.value;

        btn.elm_up    = Render.render(model.up);
        btn.elm_hover = Render.render(model.hover);
        btn.elm_down  = Render.render(model.down);

        function replace(node) {
            if (node !== btn.firstChild) {
                btn.replaceChild(node, btn.firstChild);
            }
        }
        var overCount = 0;
        function over(e) {
            if (overCount++ > 0) return;
            replace(btn.elm_hover);
        }
        function out(e) {
            if (btn.contains(e.toElement || e.relatedTarget)) return;
            overCount = 0;
            replace(btn.elm_up);
        }
        function up() {
            replace(btn.elm_hover);
            elm.notify(btn.elm_signal.id, btn.elm_value);
        }
        function down() {
            replace(btn.elm_down);
        }
        btn.addEventListener('mouseover', over);
        btn.addEventListener('mouseout' , out);
        btn.addEventListener('mousedown', down);
        btn.addEventListener('mouseup'  , up);

        btn.appendChild(btn.elm_up);

        var clicker = newNode('div');
        clicker.style.width = btn.elm_up.style.width;
        clicker.style.height = btn.elm_up.style.height;
        clicker.style.position = 'absolute';
        clicker.style.top = 0;
        btn.appendChild(clicker);

        return btn;
    }

    function updateCustomButton(node, oldModel, newModel) {
        var signal = newModel.signal;
        node.elm_up.elm_signal = signal;
        node.elm_hover.elm_signal = signal;
        node.elm_down.elm_signal = signal;

        var value = newModel.value;
        node.elm_up.elm_value = value;
        node.elm_hover.elm_value = value;
        node.elm_down.elm_value = value;

        Render.update(node.elm_up, oldModel.up, newModel.up)
        Render.update(node.elm_hover, oldModel.hover, newModel.hover)
        Render.update(node.elm_down, oldModel.down, newModel.down)
    }

    function max3(a,b,c) {
        var ab = a > b ? a : b;
        return ab > c ? ab : c;
    }

    function customButton(signal, value, up, hover, down) {
        return A3(newElement,
                  max3(up.props.width, hover.props.width, down.props.width),
                  max3(up.props.height, hover.props.height, down.props.height),
                  { ctor: 'Custom',
                    type: 'CustomButton',
                    render: renderCustomButton,
                    update: updateCustomButton,
                    model: { signal:signal, value:value, up:up, hover:hover, down:down }
                  });
    }

    function renderCheckbox(model) {
        var node = newNode('input');
        node.type = 'checkbox';
        node.checked = model.checked;
        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        function change() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.checked));
        }
        node.addEventListener('change', change);
        return node;
    }

    function updateCheckbox(node, oldModel, newModel) {
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.checked = newModel.checked;
        return true;
    }

    function checkbox(signal, handler, checked) {
        return A3(newElement, 13, 13, {
            ctor: 'Custom',
            type: 'CheckBox',
            render: renderCheckbox,
            update: updateCheckbox,
            model: { signal:signal, handler:handler, checked:checked }
        });
    }

    function setRange(node, start, end, dir) {
        if (node.parentNode) {
            node.setSelectionRange(start, end, dir);
        } else {
            setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
        }
    }

    function updateIfNeeded(css, attribute, latestAttribute) {
        if (css[attribute] !== latestAttribute) {
            css[attribute] = latestAttribute;
        }
    }
    function cssDimensions(dimensions) {
        return dimensions.top    + 'px ' +
               dimensions.right  + 'px ' +
               dimensions.bottom + 'px ' +
               dimensions.left   + 'px';
    }
    function updateFieldStyle(css, style) {
        updateIfNeeded(css, 'padding', cssDimensions(style.padding));

        var outline = style.outline;
        updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
        updateIfNeeded(css, 'border-color', toCss(outline.color));
        updateIfNeeded(css, 'border-radius', outline.radius + 'px');

        var highlight = style.highlight;
        if (highlight.width === 0) {
            css.outline = 'none';
        } else {
            updateIfNeeded(css, 'outline-width', highlight.width + 'px');
            updateIfNeeded(css, 'outline-color', toCss(highlight.color));
        }

        var textStyle = style.style;
        updateIfNeeded(css, 'color', toCss(textStyle.color));
        if (textStyle.typeface.ctor !== '[]') {
            updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
        }
        if (textStyle.height.ctor !== "Nothing") {
            updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
        }
        updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
        updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
        if (textStyle.line.ctor !== 'Nothing') {
            updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
        }
    }

    function renderField(model) {
        var field = newNode('input');
        updateFieldStyle(field.style, model.style);
        field.style.borderStyle = 'solid';
        field.style.pointerEvents = 'auto';

        field.type = model.type;
        field.placeholder = model.placeHolder;
        field.value = model.content.string;

        field.elm_signal = model.signal;
        field.elm_handler = model.handler;
        field.elm_old_value = field.value;

        function inputUpdate(event) {
            var curr = field.elm_old_value;
            var next = field.value;
            if (curr === next) {
                return;
            }

            var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
            var start = field.selectionStart;
            var end = field.selectionEnd;
            field.value = field.elm_old_value;

            elm.notify(field.elm_signal.id, field.elm_handler({
                _:{},
                string: next,
                selection: {
                    _:{},
                    start: start,
                    end: end,
                    direction: { ctor: direction }
                },
            }));
        }

        function mouseUpdate(event) {
            var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
            elm.notify(field.elm_signal.id, field.elm_handler({
                _:{},
                string: field.value,
                selection: {
                    _:{},
                    start: field.selectionStart,
                    end: field.selectionEnd,
                    direction: { ctor: direction }
                },
            }));
        }
        function mousedown(event) {
            mouseUpdate(event);
            elm.node.addEventListener('mouseup', mouseup);
        }
        function mouseup(event) {
            mouseUpdate(event);
            elm.node.removeEventListener('mouseup', mouseup)
        }
        field.addEventListener('input', inputUpdate);
        field.addEventListener('mousedown', mousedown);
        field.addEventListener('focus', function() {
            field.elm_hasFocus = true;
        });
        field.addEventListener('blur', function() {
            field.elm_hasFocus = false;
        });

        return field;
    }

    function updateField(field, oldModel, newModel) {
        if (oldModel.style !== newModel.style) {
            updateFieldStyle(field.style, newModel.style);
        }
        field.elm_signal = newModel.signal;
        field.elm_handler = newModel.handler;

        field.type = newModel.type;
        field.placeholder = newModel.placeHolder;
        var value = newModel.content.string;
        field.value = value;
        field.elm_old_value = value;
        if (field.elm_hasFocus) {
            var selection = newModel.content.selection;
            var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
            setRange(field, selection.start, selection.end, direction);
        }
    }

    function mkField(type) {
        function field(style, signal, handler, placeHolder, content) {
            var padding = style.padding;
            var outline = style.outline.width;
            var adjustWidth = padding.left + padding.right + outline.left + outline.right;
            var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
            return A3(newElement, 200, 30, {
                ctor: 'Custom',
                type: type + 'Field',
                adjustWidth: adjustWidth,
                adjustHeight: adjustHeight,
                render: renderField,
                update: updateField,
                model: {
                    signal:signal,
                    handler:handler,
                    placeHolder:placeHolder,
                    content:content,
                    style:style,
                    type:type
                }
            });
        }
        return F5(field);
    }

    function hoverable(signal, handler, elem) {
        function onHover(bool) {
            elm.notify(signal.id, handler(bool));
        }
        var props = Utils.replace([['hover',onHover]], elem.props);
        return { props:props, element:elem.element };
    }

    function clickable(signal, value, elem) {
        function onClick(bool) {
            elm.notify(signal.id, value);
        }
        var props = Utils.replace([['click',onClick]], elem.props);
        return { props:props, element:elem.element };
    }

    return elm.Native.Graphics.Input.values = {
        input:input,
        button:F3(button),
        customButton:F5(customButton),
        checkbox:F3(checkbox),
        dropDown:F2(dropDown),
        field:mkField('text'),
        email:mkField('email'),
        password:mkField('password'),
        hoverable:F3(hoverable),
        clickable:F3(clickable)
    };

};
Elm.Native.Graphics.Collage = {};
Elm.Native.Graphics.Collage.make = function(elm) {

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 elm.Native.Graphics.Collage = elm.Native.Graphics.Collage || {};
 if (elm.Native.Graphics.Collage.values) return elm.Native.Graphics.Collage.values;

 var newElement = Elm.Graphics.Element.make(elm).newElement;
 var C = ElmRuntime.use(ElmRuntime.Render.Collage);

 function collage(w,h,forms) {
     return A3(newElement, w, h, {
                 ctor: 'Custom',
		 type: 'Collage',
		 render: C.render,
		 update: C.update,
		 model: {w:w, h:h, forms:forms}
	 });
 }
 return elm.Native.Graphics.Collage.values = { collage:F3(collage) };

};Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Mouse = elm.Native.Mouse || {};
    if (elm.Native.Mouse.values) return elm.Native.Mouse.values;

    var Signal = Elm.Signal.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    var position  = Signal.constant(Utils.Tuple2(0,0));
    position.defaultNumberOfKids = 2;

    // do not move x and y into Elm. By setting their default number
    // of kids, it is possible to detatch the mouse listeners if
    // they are not needed.
    var x = A2( Signal.lift, function(p){return p._0}, position);
    x.defaultNumberOfKids = 0;
    var y = A2( Signal.lift, function(p){return p._1}, position);
    y.defaultNumberOfKids = 0;

    var isDown    = Signal.constant(false);
    var clicks = Signal.constant(Utils.Tuple0);

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    elm.addListener([clicks.id], node, 'click', function click() {
        elm.notify(clicks.id, Utils.Tuple0);
    });
    elm.addListener([isDown.id], node, 'mousedown', function down() {
        elm.notify(isDown.id, true);
    });
    elm.addListener([isDown.id], node, 'mouseup', function up() {
        elm.notify(isDown.id, false);
    });
    elm.addListener([position.id], node, 'mousemove', function move(e) {
        elm.notify(position.id, Utils.getXY(e));
    });

    return elm.Native.Mouse.values = {
        position: position,
        x:x,
        y:y,
        isDown: isDown,
        clicks: clicks
    };
};
Elm.Native.Time = {};
Elm.Native.Time.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Time = elm.Native.Time || {};
  if (elm.Native.Time.values) return elm.Native.Time.values;

  var Signal = Elm.Signal.make(elm);
  var NS = Elm.Native.Signal.make(elm);
  var Maybe = Elm.Maybe.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  function fpsWhen(desiredFPS, isOn) {
    var msPerFrame = 1000 / desiredFPS;
    var prev = Date.now(), curr = prev, diff = 0, wasOn = true;
    var ticker = Signal.constant(diff);
    function tick(zero) { return function() {
        curr = Date.now();
        diff = zero ? 0 : curr - prev;
        prev = curr;
        elm.notify(ticker.id, diff);
      };
    }
    var timeoutID = 0;
    function f(isOn, t) {
      if (isOn) {
        timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
        clearTimeout(timeoutID);
      }
      wasOn = isOn;
      return t;
    }
    return A3( Signal.lift2, F2(f), isOn, ticker );
  }

  function every(t) {
    var clock = Signal.constant(Date.now());
    setInterval(function() {
        elm.notify(clock.id, Date.now());
    }, t);
    return clock;
  }

  function since(t, s) {
    function cmp(a,b) { return !Utils.eq(a,b); }
    var dcount = Signal.count(A2(NS.delay, t, s));
    return A3( Signal.lift2, F2(cmp), Signal.count(s), dcount );
  }
  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
  }
  return elm.Native.Time.values = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Signal.constant(true)); },
      every : every,
      delay : NS.delay,
      timestamp : NS.timestamp,
      since : F2(since),
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

};
Elm.Native.WebSocket = {};
Elm.Native.WebSocket.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.WebSocket = elm.Native.WebSocket || {};
  if (elm.Native.WebSocket.values) return elm.Native.WebSocket.values;

  var Signal = Elm.Signal.make(elm);
  var List = Elm.Native.List.make(elm);

  function open(url, outgoing) {
    var incoming = Signal.constant(List.Nil);
    var ws = new WebSocket(url);

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      elm.notify(incoming.id, event.data);
    };
    
    function send(msg) {
      ready ? ws.send(msg) : pending.push(msg);
    }
    
    function take1(x,y) { return x }
    return A3(Signal.lift2, F2(take1), incoming, A2(Signal.lift, send, outgoing));
  }

  return elm.Native.WebSocket.values = { connect: F2(open) };
};
Elm.Native.Http = {};
Elm.Native.Http.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Http = elm.Native.Http || {};
    if (elm.Native.Http.values) return elm.Native.Http.values;

    var List = Elm.List.make(elm);
    var Signal = Elm.Signal.make(elm);

    function registerReq(queue,responses) {
        return function(req) {
            if (req.url.length > 0) {
                sendReq(queue,responses,req);
            }
        };
    }

    function updateQueue(queue,responses) {
        if (queue.length > 0) {
            elm.notify(responses.id, queue[0].value);
            if (queue[0].value.ctor !== 'Waiting') {
                queue.shift();
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        }
    }

    function sendReq(queue,responses,req) {
        var response = { value: { ctor:'Waiting' } };
        queue.push(response);

        var request = (window.ActiveXObject
                       ? new ActiveXObject("Microsoft.XMLHTTP")
                       : new XMLHttpRequest());

        request.onreadystatechange = function(e) {
            if (request.readyState === 4) {
                response.value = (request.status >= 200 && request.status < 300 ?
                                  { ctor:'Success', _0:request.responseText } :
                                  { ctor:'Failure', _0:request.status, _1:request.statusText });
                setTimeout(function() { updateQueue(queue,responses); }, 0);
            }
        };
        request.open(req.verb, req.url, true);
        function setHeader(pair) {
            request.setRequestHeader( pair._0, pair._1 );
        }
        A2( List.map, setHeader, req.headers );
        request.send(req.body);
    }

    function send(requests) {
        var responses = Signal.constant(elm.Http.values.Waiting);
        var sender = A2( Signal.lift, registerReq([],responses), requests );
        function f(x) { return function(y) { return x; } }
        return A3( Signal.lift2, f, responses, sender );
    }

    return elm.Native.Http.values = {
        send:send
    };
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Signal = elm.Native.Signal || {};
  if (elm.Native.Signal.values) return elm.Native.Signal.values;

  var Utils = Elm.Native.Utils.make(elm);
  var foldr1 = Elm.List.make(elm).foldr1;

  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    elm.inputs.push(this);
  }

  function LiftN(update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { isChanged = true; }
      if (count == n) {
        if (isChanged) { this.value = update(); }
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function lift(func, a) {
    function update() { return func(a.value); }
    return new LiftN(update, [a]);
  }
  function lift2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(update, [a,b]);
  }
  function lift3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(update, [a,b,c]);
  }
  function lift4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(update, [a,b,c,d]);
  }
  function lift5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(update, [a,b,c,d,e]);
  }
  function lift6(func, a, b, c, d, e, f) {
    function update() { return A6( func, a.value, b.value, c.value, d.value, e.value, f.value ); }
    return new LiftN(update, [a,b,c,d,e,f]);
  }
  function lift7(func, a, b, c, d, e, f, g) {
    function update() { return A7( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g]);
  }
  function lift8(func, a, b, c, d, e, f, g, h) {
    function update() { return A8( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g,h]);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
          this.value = A2( step, input.value, this.value );
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !pred(input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !Utils.eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(Date.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      if (parentID === s1.id) isChanged = changed;
      ++count;
      if (count == 2) {
        if (isChanged) { this.value = s2.value; }
        send(this, timestep, isChanged);
        count = 0;
        isChanged = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      function update(v) {
        if (firstEvent) { firstEvent = false; return; }
        setTimeout(function() { elm.notify(delayed.id, v); }, t);
      }
      function first(a,b) { return a; }
      return new SampleOn(delayed, lift2(F2(first), delayed, lift(update,s)));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();
      this.value = s1.value;
      this.kids = [];

      var next = null;
      var count = 0;
      var isChanged = false;

      this.recv = function(timestep, changed, parentID) {
        ++count;
        if (changed) {
            isChanged = true;
            if (parentID == s2.id && next === null) { next = s2.value; }
            if (parentID == s1.id) { next = s1.value; }
        }

        if (count == 2) {
            if (isChanged) { this.value = next; next = null; }
            send(this, timestep, isChanged);
            isChanged = false;
            count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) { return new Merge(s1,s2); }
  function merges(ss) { return A2(foldr1, F2(merge), ss); }

  return elm.Native.Signal.values = {
    constant : function(v) { return new Input(v); },
    lift  : F2(lift ),
    lift2 : F3(lift2),
    lift3 : F4(lift3),
    lift4 : F5(lift4),
    lift5 : F6(lift5),
    lift6 : F7(lift6),
    lift7 : F8(lift7),
    lift8 : F9(lift8),
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    merges : merges,
    count : function(s) { return foldp(F2(function(_,c) { return c+1; }), 0, s); },
    countIf : F2(function(pred,s) {
      return foldp(F2(function(x,c){
        return pred(x) ? c+1 : c; }), 0, s)}),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp
  };
};
Elm.Native.Random = {};
Elm.Native.Random.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Random = elm.Native.Random || {};
    if (elm.Native.Random.values) return elm.Native.Random.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);

    function range(min, max, signal) {
        function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
        return A2( Signal.lift, f, signal );
    }

    function float_(signal) {
        function f(x) { return Math.random(); }
        return A2( Signal.lift, f, signal );
    }

    function floatList(signal) {
        function f(n) {
            if (n < 0) return List.Nil;
            var arr = new Array(n);
            for (var i = n; i--; ) {
                arr[i] = Math.random();
            }
            return List.fromArray(arr);
        }
        return A2( Signal.lift, f, signal );
    }

    return elm.Native.Random.values = {
        range: F3(range),
        float_: float_,
        floatList: floatList
    };

};
Elm.Native.Window = {};
Elm.Native.Window.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Window = elm.Native.Window || {};
  if (elm.Native.Window.values) return elm.Native.Window.values;

  var Signal = Elm.Signal.make(elm);
  var Tuple2 = Elm.Native.Utils.make(elm).Tuple2;

  function getWidth() { return elm.node.clientWidth; }
  function getHeight() {
      if (elm.display === ElmRuntime.Display.FULLSCREEN) {
          return window.innerHeight;
      }
      return elm.node.clientHeight;
  }

  var dimensions = Signal.constant(Tuple2(getWidth(), getHeight()));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0;}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1;}, dimensions);
  height.defaultNumberOfKids = 0;

  function resizeIfNeeded() {
      // Do not trigger event if the dimensions have not changed.
      // This should be most of the time.
      var w = getWidth();
      var h = getHeight();
      if (dimensions.value._0 === w && dimensions.value._1 === h) return;

      setTimeout(function () {
          // Check again to see if the dimensions have changed.
          // It is conceivable that the dimensions have changed
          // again while some other event was being processed.
          var w = getWidth();
          var h = getHeight();
          if (dimensions.value._0 === w && dimensions.value._1 === h) return;
          elm.notify(dimensions.id, Tuple2(w,h));
      }, 0);
  }
  elm.addListener([dimensions.id], window, 'resize', resizeIfNeeded);

  return elm.Native.Window.values = {
      dimensions:dimensions,
      width:width,
      height:height,
      resizeIfNeeded:resizeIfNeeded
  };

};
Elm.Native.Touch = {};
Elm.Native.Touch.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Touch = elm.Native.Touch || {};
    if (elm.Native.Touch.values) return elm.Native.Touch.values;

    var Signal = Elm.Signal.make(elm);
    var List = Elm.Native.List.make(elm);
    var Utils = Elm.Native.Utils.make(elm);

    function Dict() {
        this.keys = [];
        this.values = [];

        this.insert = function(key,value) {
            this.keys.push(key);
            this.values.push(value);
        };
        this.lookup = function(key) {
            var i = this.keys.indexOf(key)
            return i >= 0 ? this.values[i] : {x:0,y:0,t:0};
        };
        this.remove = function(key) {
            var i = this.keys.indexOf(key);
            if (i < 0) return;
            var t = this.values[i];
            this.keys.splice(i,1);
            this.values.splice(i,1);
            return t;
        };
        this.clear = function() {
            this.keys = [];
            this.values = [];
        };
    }
    
    var root = Signal.constant([]),
    tapTime = 500,
    hasTap = false,
    tap = {_:{},x:0,y:0},
    dict = new Dict();

    function touch(t) {
        var r = dict.lookup(t.identifier);
        var point = Utils.getXY(t);
        return {_ : {},
	        id: t.identifier,
	        x : point._0,
	        y : point._1,
	        x0: r.x,
	        y0: r.y,
	        t0: r.t
	       };
    }

    var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

    function start(e) {
        var point = Utils.getXY(e);
        dict.insert(e.identifier,
                    {x: point._0,
                     y: point._1,
                     t: Date.now()});
    }
    function end(e) {
        var t = dict.remove(e.identifier);
        if (Date.now() - t.t < tapTime) {
            hasTap = true;
            tap = {_:{}, x:t.x, y:t.y};
        }
    }

    function listen(name, f) {
        function update(e) {
            for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
            var ts = new Array(e.touches.length);
            for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
            elm.notify(root.id, ts);
            e.preventDefault();
        }
        elm.addListener([root.id], node, name, update);
    }

    listen("touchstart", start);
    listen("touchmove", function(_){});
    listen("touchend", end);
    listen("touchcancel", end);
    listen("touchleave", end);

    var mouseID = -1;
    function move(e) {
        var point = Utils.getXY(e);
        for (var i = root.value.length; i--; ) {
            if (root.value[i].id === mouseID) {
                root.value[i].x = point._0;
                root.value[i].y = point._1;
                elm.notify(root.id, root.value);
                break;
            }
        }
    }
    elm.addListener([root.id], node, "mousedown", function down(e) {
        node.addEventListener("mousemove", move);
        e.identifier = mouseID;
        start(e);
        root.value.push(touch(e));
        elm.notify(root.id, root.value);
    });
    elm.addListener([root.id], node, "mouseup", function up(e) {
        node.removeEventListener("mousemove", move);
        e.identifier = mouseID;
        end(e);
        for (var i = root.value.length; i--; ) {
            if (root.value[i].id === mouseID) {
                root.value.splice(i, 1);
                --mouseID;
                break;
            }
        }
        elm.notify(root.id, root.value);
    });
    elm.addListener([root.id], node, "blur", function blur(e) {
        node.removeEventListener("mousemove", move);
        if (root.value.length > 0) {
            elm.notify(root.id, []);
            --mouseID;
        }
        dict.clear();
    });

    function dependency(f) {
        var sig = A2( Signal.lift, f, root );
        root.defaultNumberOfKids += 1;
        sig.defaultNumberOfKids = 0;
        return sig;
    }

    var touches = dependency(List.fromArray);

    var taps = function() {
        var sig = dependency(function(_) { return tap; });
        sig.defaultNumberOfKids = 1;
        function pred(_) { var b = hasTap; hasTap = false; return b; }
        var sig2 = A3( Signal.keepIf, pred, {_:{},x:0,y:0}, sig);
        sig2.defaultNumberOfKids = 0;
        return sig2;
    }();

    return elm.Native.Touch.values = { touches: touches, taps: taps };

};
Elm.Native.Keyboard = {};
Elm.Native.Keyboard.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Keyboard = elm.Native.Keyboard || {};
  if (elm.Native.Keyboard.values) return elm.Native.Keyboard.values;

  // Duplicated from Native.Signal
  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  var Signal = Elm.Signal.make(elm);
  var NList = Elm.Native.List.make(elm);
  var Utils = Elm.Native.Utils.make(elm);

  var downEvents = Signal.constant(0);
  var upEvents = Signal.constant(0);
  var blurEvents = Signal.constant(0);

  elm.addListener([downEvents.id], document, 'keydown', function down(e) {
    elm.notify(downEvents.id, e.keyCode);
  });

  elm.addListener([upEvents.id], document, 'keyup', function up(e) {
    elm.notify(upEvents.id, e.keyCode);
  });

  elm.addListener([blurEvents.id], document, 'blur', function blur(e) {
    elm.notify(blurEvents.id, NList.Nil);
  });

  function KeyMerge(down, up, blur) {
    var args = [down,up,blur];
    this.id = Utils.guid();
    // Ignore starting values here
    this.value = NList.Nil
    this.kids = [];
    
    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { 
        // We know this a change must only be one of the following cases
        if (parentID === down.id && !(NList.member(down.value)(this.value))) {
          isChanged = true;
          this.value = NList.Cons(down.value, this.value); 
        } 
        if (parentID === up.id) {
          isChanged = true;
          var notEq = function(kc) { return kc !== up.value };
          this.value = NList.filter(notEq)(this.value);
        } 
        if (parentID === blur.id) {
          isChanged = true;
          this.value = NList.Nil;
        }
      }
      if (count == n) {
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };

    for (var i = n; i--; ) { args[i].kids.push(this); }

  }

  var keysDown = Signal.dropRepeats(new KeyMerge(downEvents,upEvents,blurEvents));

  function keySignal(f) {
    var signal = A2(Signal.lift, f, keysDown);
    // must set the default number of kids to make it possible to filter
    // these signals if they are not actually used.
    keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 1;
    var filtered = Signal.dropRepeats(signal)
    filtered.defaultNumberOfKids = 0;
    return filtered;
  }

  function dir(up, down, left, right) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks.ctor === "::") {
        switch (ks._0) {
          case left : --x; break;
          case right: ++x; break;
          case up   : ++y; break;
          case down : --y; break;
        }
        ks = ks._1;
      }
      return { _:{}, x:x, y:y };
    }
    return keySignal(f);
  }

  function is(key) { return keySignal(NList.member(key)); }

  var lastPressed = downEvents;

  return elm.Native.Keyboard.values = {
    isDown:is,
    directions:F4(dir),
    keysDown:keysDown,
    lastPressed:lastPressed
  };

};
Elm.Bitwise = Elm.Bitwise || {};
Elm.Bitwise.make = function (_elm) {
   "use strict";
   _elm.Bitwise = _elm.Bitwise || {};
   if (_elm.Bitwise.values)
   return _elm.Bitwise.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Bitwise";
   var Native = Native || {};
   Native.Bitwise = Elm.Native.Bitwise.make(_elm);
   var _op = {};
   var shiftRightLogical = Native.Bitwise.shiftRightLogical;
   var shiftRight = Native.Bitwise.shiftRightArithmatic;
   var shiftLeft = Native.Bitwise.shiftLeft;
   var complement = Native.Bitwise.complement;
   var xor = Native.Bitwise.xor;
   var or = Native.Bitwise.or;
   var and = Native.Bitwise.and;
   _elm.Bitwise.values = {_op: _op
                         ,and: and
                         ,or: or
                         ,xor: xor
                         ,complement: complement
                         ,shiftLeft: shiftLeft
                         ,shiftRight: shiftRight
                         ,shiftRightLogical: shiftRightLogical};
   return _elm.Bitwise.values;
};Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values)
   return _elm.Random.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Random";
   var Native = Native || {};
   Native.Random = Elm.Native.Random.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var floatList = Native.Random.floatList;
   var $float = Native.Random.float_;
   var range = Native.Random.range;
   _elm.Random.values = {_op: _op
                        ,range: range
                        ,$float: $float
                        ,floatList: floatList};
   return _elm.Random.values;
};Elm.WebSocket = Elm.WebSocket || {};
Elm.WebSocket.make = function (_elm) {
   "use strict";
   _elm.WebSocket = _elm.WebSocket || {};
   if (_elm.WebSocket.values)
   return _elm.WebSocket.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "WebSocket";
   var Basics = Elm.Basics.make(_elm);
   var Native = Native || {};
   Native.WebSocket = Elm.Native.WebSocket.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var connect = Native.WebSocket.connect;
   _elm.WebSocket.values = {_op: _op
                           ,connect: connect};
   return _elm.WebSocket.values;
};Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Signal";
   var Basics = Elm.Basics.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Error = Elm.Native.Error.make(_elm);
   var Native = Native || {};
   Native.Signal = Elm.Native.Signal.make(_elm);
   var _op = {};
   _op["~"] = F2(function (sf,s) {
      return A3(Native.Signal.lift2,
      F2(function (f,x) {
         return f(x);
      }),
      sf,
      s);
   });
   _op["<~"] = F2(function (f,s) {
      return A2(Native.Signal.lift,
      f,
      s);
   });
   var sampleOn = Native.Signal.sampleOn;
   var dropRepeats = Native.Signal.dropRepeats;
   var dropIf = Native.Signal.dropIf;
   var keepIf = Native.Signal.keepIf;
   var keepWhen = F3(function (bs,
   def,
   sig) {
      return A2(_op["<~"],
      Basics.snd,
      A3(keepIf,
      Basics.fst,
      {ctor: "_Tuple2"
      ,_0: false
      ,_1: def},
      A2(_op["~"],
      A2(_op["<~"],
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      A2(sampleOn,sig,bs)),
      sig)));
   });
   var dropWhen = function (bs) {
      return keepWhen(A2(_op["<~"],
      Basics.not,
      bs));
   };
   var countIf = Native.Signal.countIf;
   var count = Native.Signal.count;
   var combine = A2(List.foldr,
   Native.Signal.lift2(F2(function (x,
   y) {
      return {ctor: "::"
             ,_0: x
             ,_1: y};
   })),
   Native.Signal.constant(_L.fromArray([])));
   var merges = Native.Signal.merges;
   var merge = Native.Signal.merge;
   var foldp = Native.Signal.foldp;
   var lift8 = Native.Signal.lift8;
   var lift7 = Native.Signal.lift7;
   var lift6 = Native.Signal.lift6;
   var lift5 = Native.Signal.lift5;
   var lift4 = Native.Signal.lift4;
   var lift3 = Native.Signal.lift3;
   var lift2 = Native.Signal.lift2;
   var lift = Native.Signal.lift;
   var constant = Native.Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,constant: constant
                        ,lift: lift
                        ,lift2: lift2
                        ,lift3: lift3
                        ,lift4: lift4
                        ,lift5: lift5
                        ,lift6: lift6
                        ,lift7: lift7
                        ,lift8: lift8
                        ,foldp: foldp
                        ,merge: merge
                        ,merges: merges
                        ,combine: combine
                        ,count: count
                        ,countIf: countIf
                        ,keepIf: keepIf
                        ,dropIf: dropIf
                        ,keepWhen: keepWhen
                        ,dropWhen: dropWhen
                        ,dropRepeats: dropRepeats
                        ,sampleOn: sampleOn
                        ,Signal: Signal};
   return _elm.Signal.values;
};Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Time";
   var Basics = Elm.Basics.make(_elm);
   var Native = Native || {};
   Native.Time = Elm.Native.Time.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var delay = Native.Time.delay;
   var timestamp = Native.Time.timestamp;
   var since = Native.Time.since;
   var every = Native.Time.every;
   var fpsWhen = Native.Time.fpsWhen;
   var fps = Native.Time.fps;
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,since: since
                      ,timestamp: timestamp
                      ,delay: delay};
   return _elm.Time.values;
};Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Dict";
   var Basics = Elm.Basics.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Error = Elm.Native.Error.make(_elm);
   var Native = Native || {};
   Native.Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _E.Case($moduleName,
         "between lines 136 and 141");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var max = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return Native.Error.raise("(max Empty) is not defined");
            case "RBNode":
            switch (t._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: t._1
                        ,_1: t._2};}
              return max(t._4);}
         _E.Case($moduleName,
         "between lines 82 and 87");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var showLColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _E.Case($moduleName,
         "between lines 62 and 64");
      }();
   };
   var empty = RBEmpty(LBlack);
   var min = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return Native.Error.raise("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (t._3.ctor)
              {case "RBEmpty":
                 switch (t._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: t._1
                             ,_1: t._2};}
                   break;}
              return min(t._3);}
         _E.Case($moduleName,
         "between lines 75 and 78");
      }();
   };
   var lookup = F2(function (k,t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v25 = A2(Native.Utils.compare,
                 k,
                 t._1);
                 switch (_v25.ctor)
                 {case "EQ":
                    return Maybe.Just(t._2);
                    case "GT": return A2(lookup,
                      k,
                      t._4);
                    case "LT": return A2(lookup,
                      k,
                      t._3);}
                 _E.Case($moduleName,
                 "between lines 93 and 99");
              }();}
         _E.Case($moduleName,
         "between lines 90 and 99");
      }();
   });
   var member = F2(function (k,t) {
      return Maybe.isJust(A2(lookup,
      k,
      t));
   });
   var findWithDefault = F3(function (base,
   k,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return base;}
              break;
            case "RBNode":
            return function () {
                 var _v33 = A2(Native.Utils.compare,
                 k,
                 t._1);
                 switch (_v33.ctor)
                 {case "EQ": return t._2;
                    case "GT":
                    return A3(findWithDefault,
                      base,
                      k,
                      t._4);
                    case "LT":
                    return A3(findWithDefault,
                      base,
                      k,
                      t._3);}
                 _E.Case($moduleName,
                 "between lines 105 and 110");
              }();}
         _E.Case($moduleName,
         "between lines 102 and 110");
      }();
   });
   var map = F2(function (f,t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              t._0,
              t._1,
              f(t._2),
              A2(map,f,t._3),
              A2(map,f,t._4));}
         _E.Case($moduleName,
         "between lines 302 and 307");
      }();
   });
   var foldl = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldl,f,acc,t._3)),
              t._4);}
         _E.Case($moduleName,
         "between lines 310 and 315");
      }();
   });
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _E.Case($moduleName,
         "between lines 318 and 323");
      }();
   });
   var keys = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return {ctor: "::"
                ,_0: k
                ,_1: acc};
      }),
      _L.fromArray([]),
      t);
   };
   var values = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return {ctor: "::"
                ,_0: v
                ,_1: acc};
      }),
      _L.fromArray([]),
      t);
   };
   var toList = function (t) {
      return A3(foldr,
      F3(function (k,v,acc) {
         return {ctor: "::"
                ,_0: {ctor: "_Tuple2"
                     ,_0: k
                     ,_1: v}
                ,_1: acc};
      }),
      _L.fromArray([]),
      t);
   };
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var isBBlack = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (t._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var Black = {ctor: "Black"};
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _E.Case($moduleName,
         "between lines 253 and 255");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 289 and 291");
      }();
   };
   var Red = {ctor: "Red"};
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _E.Case($moduleName,
         "between lines 51 and 55");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return Native.Error.raise(List.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                         ,msg
                                                         ,"and got"
                                                         ,showNColor(c)
                                                         ," "
                                                         ,lgot
                                                         ," "
                                                         ,rgot
                                                         ,"\nPlease report this bug to https://github.com/evancz/Elm/issues"])));
   });
   var ensureBlackRoot = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return t;}
              break;
            case "RBNode":
            switch (t._0.ctor)
              {case "Black": return t;
                 case "Red": return A5(RBNode,
                   Black,
                   t._1,
                   t._2,
                   t._3,
                   t._4);}
              break;}
         _E.Case($moduleName,
         "between lines 117 and 123");
      }();
   };
   var moreBlack = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack":
            return Native.Error.raise("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _E.Case($moduleName,
         "between lines 179 and 183");
      }();
   };
   var lessBlack = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return Native.Error.raise("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _E.Case($moduleName,
         "between lines 186 and 190");
      }();
   };
   var lessBlackTree = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(t._0),
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 193 and 195");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return Native.Error.raise("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _E.Case($moduleName,
         "between lines 295 and 299");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _E.Case($moduleName,
         "between lines 241 and 244");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v175 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v175.ctor)
         {case "_Tuple2":
            switch (_v175._0.ctor)
              {case "RBEmpty":
                 switch (_v175._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _E.Case($moduleName,
                           "between lines 214 and 217");
                        }();
                      case "RBNode":
                      return function () {
                           var _v197 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v175._0._0
                                       ,_2: _v175._1._0};
                           switch (_v197.ctor)
                           {case "_Tuple3":
                              switch (_v197._0.ctor)
                                {case "Black":
                                   switch (_v197._1.ctor)
                                     {case "LBlack":
                                        switch (_v197._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v175._1._1,
                                               _v175._1._2,
                                               _v175._1._3,
                                               _v175._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black, LBlack, Red",
                           c,
                           showLColor(_v175._0._0),
                           showNColor(_v175._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v175._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v201 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v175._0._0
                                       ,_2: _v175._1._0};
                           switch (_v201.ctor)
                           {case "_Tuple3":
                              switch (_v201._0.ctor)
                                {case "Black":
                                   switch (_v201._1.ctor)
                                     {case "Red":
                                        switch (_v201._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v175._0._1,
                                               _v175._0._2,
                                               _v175._0._3,
                                               _v175._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black, Red, LBlack",
                           c,
                           showNColor(_v175._0._0),
                           showLColor(_v175._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v175._0._0,
                           _v175._0._1,
                           _v175._0._2,
                           _v175._0._3,
                           _v175._0._4);
                           var r = A5(RBNode,
                           _v175._1._0,
                           _v175._1._1,
                           _v175._1._2,
                           _v175._1._3,
                           _v175._1._4);
                           var l = A5(RBNode,
                           _v175._0._0,
                           _v175._0._1,
                           _v175._0._2,
                           _v175._0._3,
                           _v175._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _E.Case($moduleName,
         "between lines 213 and 231");
      }();
   });
   var update = F3(function (k,
   u,
   t) {
      return function () {
         var up = function (t) {
            return function () {
               switch (t.ctor)
               {case "RBEmpty":
                  switch (t._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v212 = u(Maybe.Nothing);
                            switch (_v212.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v212._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _E.Case($moduleName,
                            "between lines 145 and 148");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v214 = A2(Native.Utils.compare,
                       k,
                       t._1);
                       switch (_v214.ctor)
                       {case "EQ": return function () {
                               var _v215 = u(Maybe.Just(t._2));
                               switch (_v215.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         t._0,
                                         t._1,
                                         _v215._0,
                                         t._3,
                                         t._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,t._0,t._3,t._4)};}
                               _E.Case($moduleName,
                               "between lines 149 and 152");
                            }();
                          case "GT": return function () {
                               var $ = up(t._4),
                               fl = $._0,
                               r$ = $._1;
                               return function () {
                                  switch (fl.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            t._0,
                                            t._1,
                                            t._2,
                                            t._3,
                                            r$)};}
                                  _E.Case($moduleName,
                                  "between lines 158 and 162");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(t._3),
                               fl = $._0,
                               l$ = $._1;
                               return function () {
                                  switch (fl.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            t._0,
                                            t._1,
                                            t._2,
                                            l$,
                                            t._4)};}
                                  _E.Case($moduleName,
                                  "between lines 153 and 157");
                               }();
                            }();}
                       _E.Case($moduleName,
                       "between lines 148 and 162");
                    }();}
               _E.Case($moduleName,
               "between lines 144 and 162");
            }();
         };
         var $ = up(t),
         fl = $._0,
         t$ = $._1;
         return function () {
            switch (fl.ctor)
            {case "Insert":
               return ensureBlackRoot(t$);
               case "Remove":
               return blacken(t$);
               case "Same": return t$;}
            _E.Case($moduleName,
            "between lines 163 and 168");
         }();
      }();
   });
   var insert = F3(function (k,
   v,
   t) {
      return function () {
         var u = function (_v220) {
            return function () {
               return Maybe.Just(v);
            }();
         };
         return A3(update,k,u,t);
      }();
   });
   var singleton = F2(function (k,
   v) {
      return A3(insert,
      k,
      v,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var intersect = F2(function (t1,
   t2) {
      return function () {
         var combine = F3(function (k,
         v,
         t) {
            return A2(member,
            k,
            t2) ? A3(insert,k,v,t) : t;
         });
         return A3(foldl,
         combine,
         empty,
         t1);
      }();
   });
   var fromList = function (assocs) {
      return A3(List.foldl,
      F2(function (_v222,d) {
         return function () {
            switch (_v222.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v222._0,
                 _v222._1,
                 d);}
            _E.Case($moduleName,
            "on line 353, column 43 to 55");
         }();
      }),
      empty,
      assocs);
   };
   var remove = F2(function (k,t) {
      return function () {
         var u = function (_v226) {
            return function () {
               return Maybe.Nothing;
            }();
         };
         return A3(update,k,u,t);
      }();
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,lookup: lookup
                      ,findWithDefault: findWithDefault
                      ,remove: remove
                      ,member: member
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Char";
   var Native = Native || {};
   Native.Char = Elm.Native.Char.make(_elm);
   var _op = {};
   var fromCode = Native.Char.fromCode;
   var toCode = Native.Char.toCode;
   var toLocaleLower = Native.Char.toLocaleLower;
   var toLocaleUpper = Native.Char.toLocaleUpper;
   var toLower = Native.Char.toLower;
   var toUpper = Native.Char.toUpper;
   var isHexDigit = Native.Char.isHexDigit;
   var isOctDigit = Native.Char.isOctDigit;
   var isDigit = Native.Char.isDigit;
   var isLower = Native.Char.isLower;
   var isUpper = Native.Char.isUpper;
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};Elm.Either = Elm.Either || {};
Elm.Either.make = function (_elm) {
   "use strict";
   _elm.Either = _elm.Either || {};
   if (_elm.Either.values)
   return _elm.Either.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Either";
   var List = Elm.List.make(_elm);
   var _op = {};
   var Right = function (a) {
      return {ctor: "Right",_0: a};
   };
   var isRight = function (e) {
      return function () {
         switch (e.ctor)
         {case "Right": return true;}
         return false;
      }();
   };
   var Left = function (a) {
      return {ctor: "Left",_0: a};
   };
   var either = F3(function (f,
   g,
   e) {
      return function () {
         switch (e.ctor)
         {case "Left": return f(e._0);
            case "Right": return g(e._0);}
         _E.Case($moduleName,
         "on line 37, column 16 to 60");
      }();
   });
   var isLeft = function (e) {
      return function () {
         switch (e.ctor)
         {case "Left": return true;}
         return false;
      }();
   };
   var consLeft = F2(function (e,
   vs) {
      return function () {
         switch (e.ctor)
         {case "Left": return {ctor: "::"
                              ,_0: e._0
                              ,_1: vs};
            case "Right": return vs;}
         _E.Case($moduleName,
         "between lines 78 and 80");
      }();
   });
   var lefts = function (es) {
      return A3(List.foldr,
      consLeft,
      _L.fromArray([]),
      es);
   };
   var consRight = F2(function (e,
   vs) {
      return function () {
         switch (e.ctor)
         {case "Left": return vs;
            case "Right": return {ctor: "::"
                                 ,_0: e._0
                                 ,_1: vs};}
         _E.Case($moduleName,
         "between lines 83 and 85");
      }();
   });
   var rights = function (es) {
      return A3(List.foldr,
      consRight,
      _L.fromArray([]),
      es);
   };
   var consEither = F2(function (e,
   _v13) {
      return function () {
         switch (_v13.ctor)
         {case "_Tuple2":
            return function () {
                 switch (e.ctor)
                 {case "Left":
                    return {ctor: "_Tuple2"
                           ,_0: {ctor: "::"
                                ,_0: e._0
                                ,_1: _v13._0}
                           ,_1: _v13._1};
                    case "Right":
                    return {ctor: "_Tuple2"
                           ,_0: _v13._0
                           ,_1: {ctor: "::"
                                ,_0: e._0
                                ,_1: _v13._1}};}
                 _E.Case($moduleName,
                 "between lines 88 and 90");
              }();}
         _E.Case($moduleName,
         "between lines 88 and 90");
      }();
   });
   var partition = function (es) {
      return A3(List.foldr,
      consEither,
      {ctor: "_Tuple2"
      ,_0: _L.fromArray([])
      ,_1: _L.fromArray([])},
      es);
   };
   _elm.Either.values = {_op: _op
                        ,either: either
                        ,isLeft: isLeft
                        ,isRight: isRight
                        ,lefts: lefts
                        ,rights: rights
                        ,partition: partition
                        ,consLeft: consLeft
                        ,consRight: consRight
                        ,consEither: consEither
                        ,Left: Left
                        ,Right: Right};
   return _elm.Either.values;
};Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Transform2D";
   var Native = Native || {};
   Native.Transform2D = Elm.Native.Transform2D.make(_elm);
   var _op = {};
   var multiply = Native.Transform2D.multiply;
   var rotation = Native.Transform2D.rotation;
   var matrix = Native.Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = Native.Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY
                             ,multiply: multiply
                             ,Transform2D: Transform2D};
   return _elm.Transform2D.values;
};Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Basics";
   var Native = Native || {};
   Native.Basics = Elm.Native.Basics.make(_elm);
   var _op = {};
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _E.Case($moduleName,
         "on line 381, column 19 to 24");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _E.Case($moduleName,
         "on line 365, column 13 to 14");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _E.Case($moduleName,
         "on line 361, column 13 to 14");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var id = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op["."] = F3(function (f,g,x) {
      return f(g(x));
   });
   var isInfinite = Native.Basics.isInfinite;
   var isNaN = Native.Basics.isNaN;
   var toFloat = Native.Basics.toFloat;
   var ceiling = Native.Basics.ceiling;
   var floor = Native.Basics.floor;
   var truncate = Native.Basics.truncate;
   var round = Native.Basics.round;
   var otherwise = true;
   var not = Native.Basics.not;
   var xor = Native.Basics.xor;
   _op["||"] = Native.Basics.or;
   _op["&&"] = Native.Basics.and;
   var max = Native.Basics.max;
   var min = Native.Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = Native.Basics.compare;
   _op[">="] = Native.Basics.ge;
   _op["<="] = Native.Basics.le;
   _op[">"] = Native.Basics.gt;
   _op["<"] = Native.Basics.lt;
   _op["/="] = Native.Basics.neq;
   _op["=="] = Native.Basics.eq;
   var e = Native.Basics.e;
   var pi = Native.Basics.pi;
   var clamp = Native.Basics.clamp;
   var logBase = Native.Basics.logBase;
   var abs = Native.Basics.abs;
   var sqrt = Native.Basics.sqrt;
   var atan2 = Native.Basics.atan2;
   var atan = Native.Basics.atan;
   var asin = Native.Basics.asin;
   var acos = Native.Basics.acos;
   var tan = Native.Basics.tan;
   var sin = Native.Basics.sin;
   var cos = Native.Basics.cos;
   _op["^"] = Native.Basics.exp;
   var mod = Native.Basics.mod;
   var rem = Native.Basics.rem;
   var div = Native.Basics.div;
   _op["/"] = Native.Basics.floatDiv;
   _op["*"] = Native.Basics.mul;
   _op["-"] = Native.Basics.sub;
   _op["+"] = Native.Basics.add;
   var toPolar = function (_v14) {
      return function () {
         switch (_v14.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: Native.Basics.sqrt(Math.pow(_v14._0,
                   2) + Math.pow(_v14._1,2))
                   ,_1: A2(Native.Basics.atan2,
                   _v14._1,
                   _v14._0)};}
         _E.Case($moduleName,
         "on line 79, column 18 to 73");
      }();
   };
   var fromPolar = function (_v18) {
      return function () {
         switch (_v18.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: _v18._0 * Native.Basics.cos(_v18._1)
                   ,_1: _v18._0 * Native.Basics.sin(_v18._1)};}
         _E.Case($moduleName,
         "on line 73, column 20 to 68");
      }();
   };
   var turns = function (r) {
      return 2 * Native.Basics.pi * r;
   };
   var degrees = function (d) {
      return d * Native.Basics.pi / 180;
   };
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,radians: radians
                        ,degrees: degrees
                        ,turns: turns
                        ,fromPolar: fromPolar
                        ,toPolar: toPolar
                        ,div: div
                        ,rem: rem
                        ,mod: mod
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,sqrt: sqrt
                        ,abs: abs
                        ,logBase: logBase
                        ,clamp: clamp
                        ,pi: pi
                        ,e: e
                        ,compare: compare
                        ,min: min
                        ,max: max
                        ,xor: xor
                        ,not: not
                        ,otherwise: otherwise
                        ,round: round
                        ,truncate: truncate
                        ,floor: floor
                        ,ceiling: ceiling
                        ,toFloat: toFloat
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,id: id
                        ,always: always
                        ,fst: fst
                        ,snd: snd
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};Elm.Date = Elm.Date || {};
Elm.Date.make = function (_elm) {
   "use strict";
   _elm.Date = _elm.Date || {};
   if (_elm.Date.values)
   return _elm.Date.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Date";
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Date = Elm.Native.Date.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var second = Native.Date.second;
   var minute = Native.Date.minute;
   var hour = Native.Date.hour;
   var dayOfWeek = Native.Date.dayOfWeek;
   var day = Native.Date.day;
   var month = Native.Date.month;
   var year = Native.Date.year;
   var fromTime = Native.Date.fromTime;
   var toTime = Native.Date.toTime;
   var read = Native.Date.read;
   var Dec = {ctor: "Dec"};
   var Nov = {ctor: "Nov"};
   var Oct = {ctor: "Oct"};
   var Sep = {ctor: "Sep"};
   var Aug = {ctor: "Aug"};
   var Jul = {ctor: "Jul"};
   var Jun = {ctor: "Jun"};
   var May = {ctor: "May"};
   var Apr = {ctor: "Apr"};
   var Mar = {ctor: "Mar"};
   var Feb = {ctor: "Feb"};
   var Jan = {ctor: "Jan"};
   var Sun = {ctor: "Sun"};
   var Sat = {ctor: "Sat"};
   var Fri = {ctor: "Fri"};
   var Thu = {ctor: "Thu"};
   var Wed = {ctor: "Wed"};
   var Tue = {ctor: "Tue"};
   var Mon = {ctor: "Mon"};
   var Date = {ctor: "Date"};
   _elm.Date.values = {_op: _op
                      ,read: read
                      ,toTime: toTime
                      ,fromTime: fromTime
                      ,year: year
                      ,month: month
                      ,day: day
                      ,dayOfWeek: dayOfWeek
                      ,hour: hour
                      ,minute: minute
                      ,second: second
                      ,Date: Date
                      ,Mon: Mon
                      ,Tue: Tue
                      ,Wed: Wed
                      ,Thu: Thu
                      ,Fri: Fri
                      ,Sat: Sat
                      ,Sun: Sun
                      ,Jan: Jan
                      ,Feb: Feb
                      ,Mar: Mar
                      ,Apr: Apr
                      ,May: May
                      ,Jun: Jun
                      ,Jul: Jul
                      ,Aug: Aug
                      ,Sep: Sep
                      ,Oct: Oct
                      ,Nov: Nov
                      ,Dec: Dec};
   return _elm.Date.values;
};Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Debug";
   var Native = Native || {};
   Native.Debug = Elm.Native.Debug.make(_elm);
   var _op = {};
   var log = Native.Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log};
   return _elm.Debug.values;
};Elm.Http = Elm.Http || {};
Elm.Http.make = function (_elm) {
   "use strict";
   _elm.Http = _elm.Http || {};
   if (_elm.Http.values)
   return _elm.Http.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Http";
   var Native = Native || {};
   Native.Http = Elm.Native.Http.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var send = Native.Http.send;
   var Request = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,body: c
             ,headers: d
             ,url: b
             ,verb: a};
   });
   var request = Request;
   var get = function (url) {
      return A4(Request,
      "GET",
      url,
      "",
      _L.fromArray([]));
   };
   var sendGet = function (reqs) {
      return send(A2(Signal.lift,
      get,
      reqs));
   };
   var post = F2(function (url,
   body) {
      return A4(Request,
      "POST",
      url,
      body,
      _L.fromArray([]));
   });
   var Failure = F2(function (a,
   b) {
      return {ctor: "Failure"
             ,_0: a
             ,_1: b};
   });
   var Waiting = {ctor: "Waiting"};
   var Success = function (a) {
      return {ctor: "Success"
             ,_0: a};
   };
   _elm.Http.values = {_op: _op
                      ,request: request
                      ,get: get
                      ,post: post
                      ,send: send
                      ,sendGet: sendGet
                      ,Success: Success
                      ,Waiting: Waiting
                      ,Failure: Failure
                      ,Request: Request};
   return _elm.Http.values;
};Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Text";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Show = Elm.Native.Show.make(_elm);
   var Native = Native || {};
   Native.Text = Elm.Native.Text.make(_elm);
   var String = Elm.String.make(_elm);
   var _op = {};
   var markdown = Native.Text.markdown;
   var justified = Native.Text.justified;
   var centered = Native.Text.centered;
   var rightAligned = Native.Text.rightAligned;
   var leftAligned = Native.Text.leftAligned;
   var line = Native.Text.line;
   var italic = Native.Text.italic;
   var bold = Native.Text.bold;
   var color = Native.Text.color;
   var height = Native.Text.height;
   var link = Native.Text.link;
   var monospace = Native.Text.monospace;
   var typeface = Native.Text.typeface;
   var style = Native.Text.style;
   var toText = Native.Text.toText;
   var plainText = function (str) {
      return leftAligned(toText(str));
   };
   var asText = function (value) {
      return leftAligned(monospace(toText(Native.Show.show(value))));
   };
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: Color.black
                      ,height: Maybe.Nothing
                      ,italic: false
                      ,line: Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,defaultStyle: defaultStyle
                      ,toText: toText
                      ,style: style
                      ,typeface: typeface
                      ,monospace: monospace
                      ,link: link
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,leftAligned: leftAligned
                      ,rightAligned: rightAligned
                      ,centered: centered
                      ,justified: justified
                      ,plainText: plainText
                      ,markdown: markdown
                      ,asText: asText
                      ,Text: Text
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through
                      ,Style: Style};
   return _elm.Text.values;
};Elm.Json = Elm.Json || {};
Elm.Json.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   if (_elm.Json.values)
   return _elm.Json.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Json";
   var Dict = Elm.Dict.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var _op = {};
   var fromString = function (str) {
      return Native.Json.fromString(str);
   };
   var toString = F2(function (sep,
   value) {
      return A2(Native.Json.toString,
      sep,
      value);
   });
   var Object = function (a) {
      return {ctor: "Object"
             ,_0: a};
   };
   var Array = function (a) {
      return {ctor: "Array",_0: a};
   };
   var Null = {ctor: "Null"};
   var Boolean = function (a) {
      return {ctor: "Boolean"
             ,_0: a};
   };
   var Number = function (a) {
      return {ctor: "Number"
             ,_0: a};
   };
   var String = function (a) {
      return {ctor: "String"
             ,_0: a};
   };
   _elm.Json.values = {_op: _op
                      ,toString: toString
                      ,fromString: fromString
                      ,String: String
                      ,Number: Number
                      ,Boolean: Boolean
                      ,Null: Null
                      ,Array: Array
                      ,Object: Object};
   return _elm.Json.values;
};Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "String";
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Show = Elm.Native.Show.make(_elm);
   var Native = Native || {};
   Native.String = Elm.Native.String.make(_elm);
   var _op = {};
   var fromList = Native.String.fromList;
   var toList = Native.String.toList;
   var toFloat = Native.String.toFloat;
   var toInt = Native.String.toInt;
   var show = Native.Show.show;
   var indices = Native.String.indexes;
   var indexes = Native.String.indexes;
   var endsWith = Native.String.endsWith;
   var startsWith = Native.String.startsWith;
   var contains = Native.String.contains;
   var all = Native.String.all;
   var any = Native.String.any;
   var toLower = Native.String.toLower;
   var toUpper = Native.String.toUpper;
   var lines = Native.String.lines;
   var words = Native.String.words;
   var trimRight = Native.String.trimRight;
   var trimLeft = Native.String.trimLeft;
   var trim = Native.String.trim;
   var padRight = Native.String.padRight;
   var padLeft = Native.String.padLeft;
   var pad = Native.String.pad;
   var dropRight = Native.String.dropRight;
   var dropLeft = Native.String.dropLeft;
   var right = Native.String.right;
   var left = Native.String.left;
   var sub = Native.String.sub;
   var repeat = Native.String.repeat;
   var join = Native.String.join;
   var split = Native.String.split;
   var foldr = Native.String.foldr;
   var foldl = Native.String.foldl;
   var reverse = Native.String.reverse;
   var filter = Native.String.filter;
   var map = Native.String.map;
   var length = Native.String.length;
   var concat = Native.String.concat;
   var append = Native.String.append;
   var uncons = Native.String.uncons;
   var cons = Native.String.cons;
   var isEmpty = Native.String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,cons: cons
                        ,uncons: uncons
                        ,append: append
                        ,concat: concat
                        ,length: length
                        ,map: map
                        ,filter: filter
                        ,reverse: reverse
                        ,foldl: foldl
                        ,foldr: foldr
                        ,split: split
                        ,join: join
                        ,repeat: repeat
                        ,sub: sub
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,words: words
                        ,lines: lines
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,any: any
                        ,all: all
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,show: show
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList};
   return _elm.String.values;
};Elm.Window = Elm.Window || {};
Elm.Window.make = function (_elm) {
   "use strict";
   _elm.Window = _elm.Window || {};
   if (_elm.Window.values)
   return _elm.Window.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Window";
   var Native = Native || {};
   Native.Window = Elm.Native.Window.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var height = Native.Window.height;
   var width = Native.Window.width;
   var dimensions = Native.Window.dimensions;
   _elm.Window.values = {_op: _op
                        ,dimensions: dimensions
                        ,width: width
                        ,height: height};
   return _elm.Window.values;
};Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values)
   return _elm.Regex.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Regex";
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Regex = Elm.Native.Regex.make(_elm);
   var _op = {};
   var split = Native.Regex.split;
   var replace = Native.Regex.replace;
   var find = Native.Regex.find;
   var AtMost = function (a) {
      return {ctor: "AtMost"
             ,_0: a};
   };
   var All = {ctor: "All"};
   var Match = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,index: c
             ,match: a
             ,number: d
             ,submatches: b};
   });
   var contains = Native.Regex.contains;
   var caseInsensitive = Native.Regex.caseInsensitive;
   var regex = Native.Regex.regex;
   var escape = Native.Regex.escape;
   var Regex = {ctor: "Regex"};
   _elm.Regex.values = {_op: _op
                       ,escape: escape
                       ,regex: regex
                       ,caseInsensitive: caseInsensitive
                       ,contains: contains
                       ,find: find
                       ,replace: replace
                       ,split: split
                       ,Regex: Regex
                       ,All: All
                       ,AtMost: AtMost
                       ,Match: Match};
   return _elm.Regex.values;
};Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Color";
   var Basics = Elm.Basics.make(_elm);
   var Native = Native || {};
   Native.Color = Elm.Native.Color.make(_elm);
   var _op = {};
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var hsv = Native.Color.hsv;
   var hsva = Native.Color.hsva;
   var complement = Native.Color.complement;
   var greyscale = function (p) {
      return A3(hsv,0,0,1 - p);
   };
   var grayscale = function (p) {
      return A3(hsv,0,0,1 - p);
   };
   var Color = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Color"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = Color;
   var rgb = F3(function (r,g,b) {
      return A4(Color,r,g,b,1);
   });
   var lightRed = A4(Color,
   239,
   41,
   41,
   1);
   var red = A4(Color,204,0,0,1);
   var darkRed = A4(Color,
   164,
   0,
   0,
   1);
   var lightOrange = A4(Color,
   252,
   175,
   62,
   1);
   var orange = A4(Color,
   245,
   121,
   0,
   1);
   var darkOrange = A4(Color,
   206,
   92,
   0,
   1);
   var lightYellow = A4(Color,
   255,
   233,
   79,
   1);
   var yellow = A4(Color,
   237,
   212,
   0,
   1);
   var darkYellow = A4(Color,
   196,
   160,
   0,
   1);
   var lightGreen = A4(Color,
   138,
   226,
   52,
   1);
   var green = A4(Color,
   115,
   210,
   22,
   1);
   var darkGreen = A4(Color,
   78,
   154,
   6,
   1);
   var lightBlue = A4(Color,
   114,
   159,
   207,
   1);
   var blue = A4(Color,
   52,
   101,
   164,
   1);
   var darkBlue = A4(Color,
   32,
   74,
   135,
   1);
   var lightPurple = A4(Color,
   173,
   127,
   168,
   1);
   var purple = A4(Color,
   117,
   80,
   123,
   1);
   var darkPurple = A4(Color,
   92,
   53,
   102,
   1);
   var lightBrown = A4(Color,
   233,
   185,
   110,
   1);
   var brown = A4(Color,
   193,
   125,
   17,
   1);
   var darkBrown = A4(Color,
   143,
   89,
   2,
   1);
   var black = A4(Color,0,0,0,1);
   var white = A4(Color,
   255,
   255,
   255,
   1);
   var lightGrey = A4(Color,
   238,
   238,
   236,
   1);
   var grey = A4(Color,
   211,
   215,
   207,
   1);
   var darkGrey = A4(Color,
   186,
   189,
   182,
   1);
   var lightGray = A4(Color,
   238,
   238,
   236,
   1);
   var gray = A4(Color,
   211,
   215,
   207,
   1);
   var darkGray = A4(Color,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(Color,
   136,
   138,
   133,
   1);
   var charcoal = A4(Color,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(Color,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgba: rgba
                       ,rgb: rgb
                       ,lightRed: lightRed
                       ,red: red
                       ,darkRed: darkRed
                       ,lightOrange: lightOrange
                       ,orange: orange
                       ,darkOrange: darkOrange
                       ,lightYellow: lightYellow
                       ,yellow: yellow
                       ,darkYellow: darkYellow
                       ,lightGreen: lightGreen
                       ,green: green
                       ,darkGreen: darkGreen
                       ,lightBlue: lightBlue
                       ,blue: blue
                       ,darkBlue: darkBlue
                       ,lightPurple: lightPurple
                       ,purple: purple
                       ,darkPurple: darkPurple
                       ,lightBrown: lightBrown
                       ,brown: brown
                       ,darkBrown: darkBrown
                       ,black: black
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,grayscale: grayscale
                       ,greyscale: greyscale
                       ,complement: complement
                       ,hsva: hsva
                       ,hsv: hsv
                       ,linear: linear
                       ,radial: radial
                       ,Color: Color
                       ,Linear: Linear
                       ,Radial: Radial};
   return _elm.Color.values;
};Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Maybe";
   var Basics = Elm.Basics.make(_elm);
   var List = Elm.List.make(_elm);
   var _op = {};
   var Nothing = {ctor: "Nothing"};
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var maybe = F3(function (b,
   f,
   m) {
      return function () {
         switch (m.ctor)
         {case "Just": return f(m._0);
            case "Nothing": return b;}
         _E.Case($moduleName,
         "between lines 35 and 44");
      }();
   });
   var isJust = A2(maybe,
   false,
   function (_v2) {
      return function () {
         return true;
      }();
   });
   var isNothing = function ($) {
      return Basics.not(isJust($));
   };
   var cons = F2(function (mx,xs) {
      return A3(maybe,
      xs,
      function (x) {
         return {ctor: "::"
                ,_0: x
                ,_1: xs};
      },
      mx);
   });
   var justs = A2(List.foldr,
   cons,
   _L.fromArray([]));
   _elm.Maybe.values = {_op: _op
                       ,maybe: maybe
                       ,isJust: isJust
                       ,isNothing: isNothing
                       ,cons: cons
                       ,justs: justs
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};Elm.Touch = Elm.Touch || {};
Elm.Touch.make = function (_elm) {
   "use strict";
   _elm.Touch = _elm.Touch || {};
   if (_elm.Touch.values)
   return _elm.Touch.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Touch";
   var Native = Native || {};
   Native.Touch = Elm.Native.Touch.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var taps = Native.Touch.taps;
   var touches = Native.Touch.touches;
   var Touch = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,id: c
             ,t0: f
             ,x: a
             ,x0: d
             ,y: b
             ,y0: e};
   });
   _elm.Touch.values = {_op: _op
                       ,touches: touches
                       ,taps: taps
                       ,Touch: Touch};
   return _elm.Touch.values;
};Elm.Mouse = Elm.Mouse || {};
Elm.Mouse.make = function (_elm) {
   "use strict";
   _elm.Mouse = _elm.Mouse || {};
   if (_elm.Mouse.values)
   return _elm.Mouse.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Mouse";
   var Native = Native || {};
   Native.Mouse = Elm.Native.Mouse.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var clicks = Native.Mouse.clicks;
   var isDown = Native.Mouse.isDown;
   var y = Native.Mouse.y;
   var x = Native.Mouse.x;
   var position = Native.Mouse.position;
   _elm.Mouse.values = {_op: _op
                       ,position: position
                       ,x: x
                       ,y: y
                       ,isDown: isDown
                       ,clicks: clicks};
   return _elm.Mouse.values;
};Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Array";
   var Basics = Elm.Basics.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Array = Elm.Native.Array.make(_elm);
   var _op = {};
   var concat = Native.Array.concat;
   var length = Native.Array.length;
   var slice = Native.Array.slice;
   var updates = F2(function (us,
   array) {
      return A3(List.foldl,
      Basics.uncurry(Native.Array.set),
      array,
      us);
   });
   var set = Native.Array.set;
   var getSafe = F3(function ($default,
   i,
   array) {
      return _U.cmp(Native.Array.length(array),
      i) > 0 ? A2(Native.Array.get,
      i,
      array) : $default;
   });
   var getMaybe = F2(function (i,
   array) {
      return _U.cmp(Native.Array.length(array),
      i) > 0 ? Maybe.Just(A2(Native.Array.get,
      i,
      array)) : Maybe.Nothing;
   });
   var get = Native.Array.get;
   var push = Native.Array.push;
   var empty = Native.Array.empty;
   var foldr = Native.Array.foldr;
   var foldl = Native.Array.foldl;
   var indexedMap = Native.Array.indexedMap;
   var map = Native.Array.map;
   var indices = function (a) {
      return _L.range(0,
      Native.Array.length(a) - 1);
   };
   var toIndexedList = function (a) {
      return A2(List.zip,
      indices(a),
      Native.Array.toList(a));
   };
   var toList = Native.Array.toList;
   var fromList = A2(List.foldl,
   Native.Array.push,
   Native.Array.empty);
   var zipWith = F3(function (f,
   a,
   b) {
      return fromList(A3(List.zipWith,
      f,
      Native.Array.toList(a),
      Native.Array.toList(b)));
   });
   var zip = zipWith(F2(function (v0,
   v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }));
   var fill = F2(function (len,e) {
      return fromList(A2(List.map,
      Basics.always(e),
      _L.range(1,len)));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,fill: fill
                       ,fromList: fromList
                       ,toList: toList
                       ,indices: indices
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,foldl: foldl
                       ,foldr: foldr
                       ,zip: zip
                       ,zipWith: zipWith
                       ,empty: empty
                       ,push: push
                       ,get: get
                       ,getMaybe: getMaybe
                       ,getSafe: getSafe
                       ,set: set
                       ,updates: updates
                       ,slice: slice
                       ,length: length
                       ,concat: concat
                       ,Array: Array};
   return _elm.Array.values;
};Elm.Trampoline = Elm.Trampoline || {};
Elm.Trampoline.make = function (_elm) {
   "use strict";
   _elm.Trampoline = _elm.Trampoline || {};
   if (_elm.Trampoline.values)
   return _elm.Trampoline.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Trampoline";
   var Native = Native || {};
   Native.Trampoline = Elm.Native.Trampoline.make(_elm);
   var _op = {};
   var trampoline = Native.Trampoline.trampoline;
   var Continue = function (a) {
      return {ctor: "Continue"
             ,_0: a};
   };
   var Done = function (a) {
      return {ctor: "Done",_0: a};
   };
   _elm.Trampoline.values = {_op: _op
                            ,trampoline: trampoline
                            ,Done: Done
                            ,Continue: Continue};
   return _elm.Trampoline.values;
};Elm.Keyboard = Elm.Keyboard || {};
Elm.Keyboard.make = function (_elm) {
   "use strict";
   _elm.Keyboard = _elm.Keyboard || {};
   if (_elm.Keyboard.values)
   return _elm.Keyboard.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Keyboard";
   var Native = Native || {};
   Native.Keyboard = Elm.Native.Keyboard.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var lastPressed = Native.Keyboard.lastPressed;
   var keysDown = Native.Keyboard.keysDown;
   var isDown = Native.Keyboard.isDown;
   var shift = isDown(16);
   var ctrl = isDown(17);
   var space = isDown(32);
   var enter = isDown(13);
   var directions = Native.Keyboard.directions;
   var arrows = A4(directions,
   38,
   40,
   37,
   39);
   var wasd = A4(directions,
   87,
   83,
   65,
   68);
   _elm.Keyboard.values = {_op: _op
                          ,directions: directions
                          ,arrows: arrows
                          ,wasd: wasd
                          ,isDown: isDown
                          ,shift: shift
                          ,ctrl: ctrl
                          ,space: space
                          ,enter: enter
                          ,keysDown: keysDown
                          ,lastPressed: lastPressed};
   return _elm.Keyboard.values;
};Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values)
   return _elm.Set.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Set";
   var Dict = Elm.Dict.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var _op = {};
   var foldr = F3(function (f,
   b,
   s) {
      return A3(Dict.foldr,
      F3(function (k,_v0,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var foldl = F3(function (f,
   b,
   s) {
      return A3(Dict.foldl,
      F3(function (k,_v2,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var toList = Dict.keys;
   var diff = Dict.diff;
   var intersect = Dict.intersect;
   var union = Dict.union;
   var member = Dict.member;
   var remove = Dict.remove;
   var insert = function (k) {
      return A2(Dict.insert,
      k,
      {ctor: "_Tuple0"});
   };
   var singleton = function (k) {
      return A2(Dict.singleton,
      k,
      {ctor: "_Tuple0"});
   };
   var empty = Dict.empty;
   var fromList = function (xs) {
      return A3(List.foldl,
      insert,
      empty,
      xs);
   };
   var map = F2(function (f,s) {
      return fromList(A2(List.map,
      f,
      toList(s)));
   });
   _elm.Set.values = {_op: _op
                     ,empty: empty
                     ,singleton: singleton
                     ,insert: insert
                     ,remove: remove
                     ,member: member
                     ,foldl: foldl
                     ,foldr: foldr
                     ,map: map
                     ,union: union
                     ,intersect: intersect
                     ,diff: diff
                     ,toList: toList
                     ,fromList: fromList};
   return _elm.Set.values;
};Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "List";
   var Basics = Elm.Basics.make(_elm);
   var Native = Native || {};
   Native.List = Elm.Native.List.make(_elm);
   var _op = {};
   var sortWith = Native.List.sortWith;
   var sortBy = Native.List.sortBy;
   var sort = Native.List.sort;
   var repeat = Native.List.repeat;
   var drop = Native.List.drop;
   var take = Native.List.take;
   var join = Native.List.join;
   var zipWith = Native.List.zipWith;
   var zip = Native.List.zip;
   var concat = Native.List.concat;
   var any = Native.List.any;
   var all = Native.List.all;
   var reverse = Native.List.reverse;
   var length = Native.List.length;
   var filter = Native.List.filter;
   var scanl1 = Native.List.scanl1;
   var scanl = Native.List.scanl;
   var foldr1 = Native.List.foldr1;
   var foldl1 = Native.List.foldl1;
   var maximum = foldl1(Basics.max);
   var minimum = foldl1(Basics.min);
   var foldr = Native.List.foldr;
   var foldl = Native.List.foldl;
   var and = A2(foldl,
   F2(function (x,y) {
      return x && y;
   }),
   true);
   var or = A2(foldl,
   F2(function (x,y) {
      return x || y;
   }),
   false);
   var sum = A2(foldl,
   F2(function (x,y) {
      return x + y;
   }),
   0);
   var product = A2(foldl,
   F2(function (x,y) {
      return x * y;
   }),
   1);
   var map = Native.List.map;
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var last = Native.List.last;
   var tail = Native.List.tail;
   var head = Native.List.head;
   _op["++"] = Native.List.append;
   _op["::"] = Native.List.cons;
   var partition = F2(function (pred,
   lst) {
      return function () {
         switch (lst.ctor)
         {case "::": return function () {
                 var $ = A2(partition,
                 pred,
                 lst._1),
                 bs = $._0,
                 cs = $._1;
                 return pred(lst._0) ? {ctor: "_Tuple2"
                                       ,_0: {ctor: "::"
                                            ,_0: lst._0
                                            ,_1: bs}
                                       ,_1: cs} : {ctor: "_Tuple2"
                                                  ,_0: bs
                                                  ,_1: {ctor: "::"
                                                       ,_0: lst._0
                                                       ,_1: cs}};
              }();
            case "[]":
            return {ctor: "_Tuple2"
                   ,_0: _L.fromArray([])
                   ,_1: _L.fromArray([])};}
         _E.Case($moduleName,
         "between lines 169 and 179");
      }();
   });
   var unzip = function (pairs) {
      return function () {
         switch (pairs.ctor)
         {case "::":
            switch (pairs._0.ctor)
              {case "_Tuple2":
                 return function () {
                      var $ = unzip(pairs._1),
                      xs = $._0,
                      ys = $._1;
                      return {ctor: "_Tuple2"
                             ,_0: {ctor: "::"
                                  ,_0: pairs._0._0
                                  ,_1: xs}
                             ,_1: {ctor: "::"
                                  ,_0: pairs._0._1
                                  ,_1: ys}};
                   }();}
              break;
            case "[]":
            return {ctor: "_Tuple2"
                   ,_0: _L.fromArray([])
                   ,_1: _L.fromArray([])};}
         _E.Case($moduleName,
         "between lines 194 and 202");
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": switch (xs._1.ctor)
              {case "::": return {ctor: "::"
                                 ,_0: xs._0
                                 ,_1: {ctor: "::"
                                      ,_0: sep
                                      ,_1: A2(intersperse,
                                      sep,
                                      {ctor: "::"
                                      ,_0: xs._1._0
                                      ,_1: xs._1._1})}};
                 case "[]":
                 return _L.fromArray([xs._0]);}
              break;
            case "[]":
            return _L.fromArray([]);}
         _E.Case($moduleName,
         "between lines 212 and 217");
      }();
   });
   _elm.List.values = {_op: _op
                      ,head: head
                      ,tail: tail
                      ,last: last
                      ,isEmpty: isEmpty
                      ,map: map
                      ,foldl: foldl
                      ,foldr: foldr
                      ,foldl1: foldl1
                      ,foldr1: foldr1
                      ,scanl: scanl
                      ,scanl1: scanl1
                      ,filter: filter
                      ,length: length
                      ,reverse: reverse
                      ,all: all
                      ,any: any
                      ,and: and
                      ,or: or
                      ,concat: concat
                      ,concatMap: concatMap
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,partition: partition
                      ,zip: zip
                      ,zipWith: zipWith
                      ,unzip: unzip
                      ,join: join
                      ,intersperse: intersperse
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};Elm.JavaScript = Elm.JavaScript || {};
Elm.JavaScript.Experimental = Elm.JavaScript.Experimental || {};
Elm.JavaScript.Experimental.make = function (_elm) {
   "use strict";
   _elm.JavaScript = _elm.JavaScript || {};
   _elm.JavaScript.Experimental = _elm.JavaScript.Experimental || {};
   if (_elm.JavaScript.Experimental.values)
   return _elm.JavaScript.Experimental.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "JavaScript.Experimental";
   var Json = Elm.Json.make(_elm);
   var Native = Native || {};
   Native.JavaScript = Elm.Native.JavaScript.make(_elm);
   var _op = {};
   var fromJson = Native.Json.toJS;
   var toJson = Native.Json.fromJS;
   var fromRecord = Native.JavaScript.fromRecord;
   var toRecord = Native.JavaScript.toRecord;
   var RawObject = {ctor: "RawObject"};
   _elm.JavaScript.Experimental.values = {_op: _op
                                         ,toRecord: toRecord
                                         ,fromRecord: fromRecord
                                         ,toJson: toJson
                                         ,fromJson: fromJson
                                         ,RawObject: RawObject};
   return _elm.JavaScript.Experimental.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Collage";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Either = Elm.Either.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Graphics = Native.Graphics || {};
   Native.Graphics.Collage = Elm.Native.Graphics.Collage.make(_elm);
   var Transform2D = Elm.Transform2D.make(_elm);
   var _op = {};
   var ngon = F2(function (n,r) {
      return function () {
         var m = Basics.toFloat(n);
         var t = 2 * Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * Basics.cos(t * i)
                   ,_1: r * Basics.sin(t * i)};
         };
         return A2(List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * Basics.cos(t * i)
                   ,_1: hh * Basics.sin(t * i)};
         };
         return A2(List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = Native.Graphics.Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _E.Case($moduleName,
         "on line 169, column 20 to 48");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Either.Right(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Either.Left(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,defaultLine: defaultLine
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,form: form
                                  ,fill: fill
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,sprite: sprite
                                  ,toForm: toForm
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,collage: collage
                                  ,path: path
                                  ,segment: segment
                                  ,polygon: polygon
                                  ,rect: rect
                                  ,square: square
                                  ,oval: oval
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,Solid: Solid
                                  ,Texture: Texture
                                  ,Grad: Grad
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped
                                  ,FPath: FPath
                                  ,FShape: FShape
                                  ,FImage: FImage
                                  ,FElement: FElement
                                  ,FGroup: FGroup
                                  ,Form: Form
                                  ,LineStyle: LineStyle};
   return _elm.Graphics.Collage.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   if (_elm.Graphics.Input.values)
   return _elm.Graphics.Input.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Input";
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Native = Native || {};
   Native.Graphics = Native.Graphics || {};
   Native.Graphics.Input = Elm.Native.Graphics.Input.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var _op = {};
   var clickable = Native.Graphics.Input.clickable;
   var hoverable = Native.Graphics.Input.hoverable;
   var dropDown = Native.Graphics.Input.dropDown;
   var checkbox = Native.Graphics.Input.checkbox;
   var customButton = Native.Graphics.Input.customButton;
   var button = Native.Graphics.Input.button;
   var input = Native.Graphics.Input.input;
   var Handle = {ctor: "Handle"};
   var Input = F2(function (a,b) {
      return {_: {}
             ,handle: b
             ,signal: a};
   });
   _elm.Graphics.Input.values = {_op: _op
                                ,input: input
                                ,button: button
                                ,customButton: customButton
                                ,checkbox: checkbox
                                ,dropDown: dropDown
                                ,hoverable: hoverable
                                ,clickable: clickable
                                ,Handle: Handle
                                ,Input: Input};
   return _elm.Graphics.Input.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Element";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,Basics.round(Basics.toFloat(_v0._1) / Basics.toFloat(_v0._2) * Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,Basics.round(Basics.toFloat(_v5._2) / Basics.toFloat(_v5._1) * Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,Basics.snd(A2(Native.Utils.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var newElement = F3(function (w,
   h,
   e) {
      return {_: {}
             ,element: e
             ,props: A9(Properties,
             Native.Utils.guid({ctor: "_Tuple0"}),
             w,
             h,
             1,
             Maybe.Nothing,
             "",
             "",
             {ctor: "_Tuple0"},
             {ctor: "_Tuple0"})};
   });
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var hs = A2(List.map,
         heightOf,
         es);
         var ws = A2(List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 List.maximum(ws),
                 List.sum(hs));
               case "DIn": return A2(newFlow,
                 List.maximum(ws),
                 List.maximum(hs));
               case "DLeft": return A2(newFlow,
                 List.sum(ws),
                 List.maximum(hs));
               case "DOut": return A2(newFlow,
                 List.maximum(ws),
                 List.maximum(hs));
               case "DRight":
               return A2(newFlow,
                 List.sum(ws),
                 List.maximum(hs));
               case "DUp": return A2(newFlow,
                 List.maximum(ws),
                 List.sum(hs));}
            _E.Case($moduleName,
            "between lines 206 and 216");
         }();
      }();
   });
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2(Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2(Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2(Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2(List.map,
         heightOf,
         es);
         var ws = A2(List.map,
         widthOf,
         es);
         return A3(newElement,
         List.maximum(ws),
         List.maximum(hs),
         A2(Flow,DOut,es));
      }();
   };
   _elm.Graphics.Element.values = {_op: _op
                                  ,empty: empty
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,opacity: opacity
                                  ,color: color
                                  ,tag: tag
                                  ,link: link
                                  ,newElement: newElement
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,container: container
                                  ,spacer: spacer
                                  ,flow: flow
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,layers: layers
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middle: middle
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,middleAt: middleAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,Image: Image
                                  ,Container: Container
                                  ,Flow: Flow
                                  ,Spacer: Spacer
                                  ,RawHtml: RawHtml
                                  ,Custom: Custom
                                  ,Plain: Plain
                                  ,Fitted: Fitted
                                  ,Cropped: Cropped
                                  ,Tiled: Tiled
                                  ,P: P
                                  ,Z: Z
                                  ,N: N
                                  ,Absolute: Absolute
                                  ,Relative: Relative
                                  ,DUp: DUp
                                  ,DDown: DDown
                                  ,DLeft: DLeft
                                  ,DRight: DRight
                                  ,DIn: DIn
                                  ,DOut: DOut
                                  ,Properties: Properties
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.Field = Elm.Graphics.Input.Field || {};
Elm.Graphics.Input.Field.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   _elm.Graphics.Input.Field = _elm.Graphics.Input.Field || {};
   if (_elm.Graphics.Input.Field.values)
   return _elm.Graphics.Input.Field.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Graphics.Input.Field";
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Input = Elm.Graphics.Input.make(_elm);
   var Native = Native || {};
   Native.Graphics = Native.Graphics || {};
   Native.Graphics.Input = Elm.Native.Graphics.Input.make(_elm);
   var Text = Elm.Text.make(_elm);
   var _op = {};
   var email = Native.Graphics.Input.email;
   var password = Native.Graphics.Input.password;
   var field = Native.Graphics.Input.field;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var Selection = F3(function (a,
   b,
   c) {
      return {_: {}
             ,direction: c
             ,end: b
             ,start: a};
   });
   var Content = F2(function (a,
   b) {
      return {_: {}
             ,selection: b
             ,string: a};
   });
   var noContent = A2(Content,
   "",
   A3(Selection,0,0,Forward));
   var Style = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,highlight: c
             ,outline: b
             ,padding: a
             ,style: d};
   });
   var Highlight = F2(function (a,
   b) {
      return {_: {}
             ,color: a
             ,width: b};
   });
   var noHighlight = A2(Highlight,
   Color.blue,
   0);
   var Outline = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: a
             ,radius: c
             ,width: b};
   });
   var Dimensions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: a
             ,right: b
             ,top: c};
   });
   var uniformly = function (n) {
      return A4(Dimensions,
      n,
      n,
      n,
      n);
   };
   var noOutline = A3(Outline,
   Color.grey,
   uniformly(0),
   0);
   var defaultStyle = {_: {}
                      ,highlight: A2(Highlight,
                      Color.blue,
                      1)
                      ,outline: A3(Outline,
                      Color.grey,
                      uniformly(1),
                      2)
                      ,padding: uniformly(4)
                      ,style: Text.defaultStyle};
   _elm.Graphics.Input.Field.values = {_op: _op
                                      ,uniformly: uniformly
                                      ,noOutline: noOutline
                                      ,noHighlight: noHighlight
                                      ,defaultStyle: defaultStyle
                                      ,noContent: noContent
                                      ,field: field
                                      ,password: password
                                      ,email: email
                                      ,Forward: Forward
                                      ,Backward: Backward
                                      ,Dimensions: Dimensions
                                      ,Outline: Outline
                                      ,Highlight: Highlight
                                      ,Style: Style
                                      ,Content: Content
                                      ,Selection: Selection};
   return _elm.Graphics.Input.Field.values;
};
(function() {
'use strict';

Elm.fullscreen = function(module, ports) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module, ports || {});
};

Elm.embed = function(module, container, ports) {
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
    } else if (container.hasChildNodes()) {
        throw new Error('Elm.node must be given an empty DIV. No children allowed!');
    }
    return init(ElmRuntime.Display.COMPONENT, container, module, ports || {});
};

Elm.worker = function(module, ports) {
    return init(ElmRuntime.Display.NONE, {}, module, ports || {});
};

function init(display, container, module, ports, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  var updateInProgress = false;
  function notify(id, v) {
      if (updateInProgress) {
          throw new Error(
              'The notify function has been called synchronously!\n' +
              'This can lead to frames being dropped.\n' +
              'Definitely report this to <https://github.com/evancz/Elm/issues>\n');
      }
      updateInProgress = true;
      var timestep = Date.now();
      for (var i = inputs.length; i--; ) {
          inputs[i].recv(timestep, id, v);
      }
      updateInProgress = false;
  }

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  var portUses = {}
  for (var key in ports) {
      portUses[key] = 0;
  }
  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {
      notify:notify,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs,
      ports: { incoming:ports, outgoing:{}, uses:portUses }
  };

  function swap(newModule) {
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, ports, elm);
      inputs = [];
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = {};
  var reportAnyErrors = function() {};
  try {
      Module = module.make(elm);
      checkPorts(elm);
  } catch(e) {
      var directions = "<br/>&nbsp; &nbsp; Open the developer console for more details."
      Module.main = Elm.Text.make(elm).leftAligned('<code>' + e.message + directions + '</code>');
      reportAnyErrors = function() { throw e; }
  }
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  addReceivers(elm.ports.outgoing);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.recv(0, true, 0);
      }
  }

  reportAnyErrors();
  return { swap:swap, ports:elm.ports.outgoing };
};

function checkPorts(elm) {
    var portUses = elm.ports.uses;
    for (var key in portUses) {
        var uses = portUses[key]
        if (uses === 0) {
            throw new Error(
                "Initialization Error: provided port '" + key +
                "' to a module that does not take it as in input.\n" +
                "Remove '" + key + "' from the module initialization code.");
        } else if (uses > 1) {
            throw new Error(
                "Initialization Error: port '" + key +
                "' has been declared multiple times in the Elm code.\n" +
                "Remove declarations until there is exactly one.");
        }
    }
}
    
function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

// add receivers for built-in ports if they are defined
function addReceivers(ports) {
    if ('log' in ports) {
        ports.log.subscribe(function(v) { console.log(v) });
    }
    if ('stdout' in ports) {
        var process = process || {};
        var handler = process.stdout
            ? function(v) { process.stdout.write(v); }
            : function(v) { console.log(v); };
        ports.stdout.subscribe(handler);
    }
    if ('stderr' in ports) {
        var process = process || {};
        var handler = process.stderr
            ? function(v) { process.stderr.write(v); }
            : function(v) { console.log('Error:' + v); };
        ports.stderr.subscribe(handler);
    }
    if ('title' in ports) {
        if (typeof ports.title === 'string') {
            document.title = ports.title;
        } else {
            ports.title.subscribe(function(v) { document.title = v; });
        }
    }
    if ('redirect' in ports) {
        ports.redirect.subscribe(function(v) {
            if (v.length > 0) window.location = v;
        });
    }
    if ('favicon' in ports) {
        if (typeof ports.favicon === 'string') {
            changeFavicon(ports.favicon);
        } else {
            ports.favicon.subscribe(changeFavicon);
        }
    }
    function changeFavicon(src) {
        var link = document.createElement('link');
        var oldLink = document.getElementById('elm-favicon');
        link.id = 'elm-favicon';
        link.rel = 'shortcut icon';
        link.href = src;
        if (oldLink) {
            document.head.removeChild(oldLink);
        }
        document.head.appendChild(link);
    }
}

function initGraphics(elm, Module) {
  if (!('main' in Module))
      throw new Error("'main' is missing! What do I display?!");

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal.make(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;

 // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));

  // set up updates so that the DOM is adjusted as necessary.
  var savedScene = currentScene;
  function domUpdate(newScene) {
      ElmRuntime.draw(function(_) {
          Render.update(elm.node.firstChild, savedScene, newScene);
          if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();
          savedScene = newScene;
      });
  }
  var renderer = A2(Signal.lift, domUpdate, signalGraph);

  // must check for resize after 'renderer' is created so
  // that changes show up.
  if (elm.Native.Window) elm.Native.Window.values.resizeIfNeeded();

  return renderer;
}

}());

(function() {
'use strict';

ElmRuntime.Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

ElmRuntime.counter = 0;
ElmRuntime.guid = function() { return ElmRuntime.counter++; }

ElmRuntime.use = function(M) {
    if (typeof M === 'function') M = M();
    return M;
};

function isAlive(input) {
    if (!('defaultNumberOfKids' in input)) return true;
    var len = input.kids.length;
    if (len === 0) return false;
    if (len > input.defaultNumberOfKids) return true;
    var alive = false;
    for (var i = len; i--; ) {
        alive = alive || isAlive(input.kids[i]);
    }
    return alive;
}

ElmRuntime.filterDeadInputs = function(inputs) {
    var temp = [];
    for (var i = inputs.length; i--; ) {
        if (isAlive(inputs[i])) temp.push(inputs[i]);
    }
    return temp;
};

// define the draw function
var vendors = ['ms', 'moz', 'webkit', 'o'];
var win = typeof window !== 'undefined' ? window : {};
for (var i = 0; i < vendors.length && !win.requestAnimationFrame; ++i) {
    win.requestAnimationFrame = win[vendors[i]+'RequestAnimationFrame'];
    win.cancelAnimationFrame  = win[vendors[i]+'CancelAnimationFrame'] ||
                                win[vendors[i]+'CancelRequestAnimationFrame'];
}

if (win.requestAnimationFrame && win.cancelAnimationFrame) {
    var previous = 0;
    ElmRuntime.draw = function(callback) {
        win.cancelAnimationFrame(previous);
        previous = win.requestAnimationFrame(callback);
    };
} else {
    ElmRuntime.draw = function(callback) { callback(); };
}

}());
(function() {

// Returns boolean indicating if the swap was successful.
// Requires that the two signal graphs have exactly the same
// structure.
ElmRuntime.swap = function(from, to) {
    function similar(nodeOld,nodeNew) {
        var idOkay = nodeOld.id === nodeNew.id;
        var lengthOkay = nodeOld.kids.length === nodeNew.kids.length;
        return idOkay && lengthOkay;
    }
    function swap(nodeOld,nodeNew) {
        nodeNew.value = nodeOld.value;
        return true;
    }
    var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
    if (canSwap) { depthFirstTraversals(swap, from.inputs, to.inputs); }
    from.node.parentNode.replaceChild(to.node, from.node);
    return canSwap;
}

// Returns false if the node operation f ever fails.
function depthFirstTraversals(f, queueOld, queueNew) {
    if (queueOld.length !== queueNew.length) return false;
    queueOld = queueOld.slice(0);
    queueNew = queueNew.slice(0);

    var seen = [];
    while (queueOld.length > 0 && queueNew.length > 0) {
        var nodeOld = queueOld.pop();
        var nodeNew = queueNew.pop();
        if (seen.indexOf(nodeOld.id) < 0) {
            if (!f(nodeOld, nodeNew)) return false;
            queueOld = queueOld.concat(nodeOld.kids);
            queueNew = queueNew.concat(nodeNew.kids);
            seen.push(nodeOld.id);
        }
    }
    return true;
}

}());

ElmRuntime.Render.Element = function() {
'use strict';

var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement, extract = Utils.extract,
    addTransform = Utils.addTransform, removeTransform = Utils.removeTransform,
    fromList = Utils.fromList, eq = Utils.eq;

function setProps(elem, e) {
    var props = elem.props;
    var element = elem.element;
    var width = props.width - (element.adjustWidth || 0);
    var height = props.height - (element.adjustHeight || 0);
    e.style.width  = (width |0) + 'px';
    e.style.height = (height|0) + 'px';
    if (props.opacity !== 1) {
        e.style.opacity = props.opacity;
    }
    if (props.color.ctor === 'Just') {
        e.style.backgroundColor = extract(props.color._0);
    }
    if (props.tag !== '') { e.id = props.tag; }
    if (props.href !== '') {
        var a = newElement('a');
        a.href = props.href;
        a.style.width = '100%';
        a.style.height = '100%';
        a.style.top = 0;
        a.style.left = 0;
        a.style.display = 'block';
        a.style.position = 'absolute';
        e.style.position = 'relative';
        e.appendChild(a);
    }
    if (props.hover.ctor !== '_Tuple0') {
        addHover(e, props.hover);
    }
    if (props.click.ctor !== '_Tuple0') {
        addClick(e, props.click);
    }
    return e;
}

function addClick(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_click_handler = handler;
    function trigger() {
        e.elm_click_handler(Utils.Tuple0);
    }
    e.elm_click_trigger = trigger;
    e.addEventListener('click', trigger);
}

function removeClick(e, handler) {
    if (e.elm_click_trigger) {
        e.removeEventListener('click', e.elm_click_trigger);
    }
}

function addHover(e, handler) {
    e.style.pointerEvents = 'auto';
    e.elm_hover_handler = handler;
    e.elm_hover_count = 0;

    function over() {
        if (e.elm_hover_count++ > 0) return;
        e.elm_hover_handler(true);
    }
    function out(evt) {
        if (e.contains(evt.toElement || evt.relatedTarget)) return;
        e.elm_hover_count = 0;
        e.elm_hover_handler(false);
    }
    e.elm_hover_over = over;
    e.elm_hover_out = out;
    e.addEventListener('mouseover', over);
    e.addEventListener('mouseout', out);
}

function removeHover(e) {
    if (e.elm_hover_over) {
        e.removeEventListener('mouseover', e.elm_hover_over);
    }
    if (e.elm_hover_out) {
        e.removeEventListener('mouseout', e.elm_hover_out);
    }
}

function image(props, img) {
    switch (img._0.ctor) {
    case 'Plain':   return plainImage(img._3);
    case 'Fitted':  return fittedImage(props.width, props.height, img._3);
    case 'Cropped': return croppedImage(img,props.width,props.height,img._3);
    case 'Tiled':   return tiledImage(img._3);
    }
}

function plainImage(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function tiledImage(src) {
    var div = newElement('div');
    div.style.backgroundImage = 'url(' + src + ')';
    return div;
}

function fittedImage(w, h, src) {
    var div = newElement('div');
    div.style.background = 'url(' + src + ') no-repeat center';
    div.style.webkitBackgroundSize = 'cover';
    div.style.MozBackgroundSize = 'cover';
    div.style.OBackgroundSize = 'cover';
    div.style.backgroundSize = 'cover';
    return div;
}

function croppedImage(elem, w, h, src) {
    var pos = elem._0._0;
    var e = newElement('div');
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
        var sw = w / elem._1, sh = h / elem._2;
        img.style.width = ((this.width * sw)|0) + 'px';
        img.style.height = ((this.height * sh)|0) + 'px';
        img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
        img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
    };
    img.src = src;
    img.name = src;
    e.appendChild(img);
    return e;
}

function goIn(e) { e.style.position = 'absolute'; return e; }
function goDown(e) { return e }
function goRight(e) { e.style.styleFloat = e.style.cssFloat = "left"; return e; }
function flowWith(f, array) {
    var container = newElement('div');
    if (f == goIn) container.style.pointerEvents = 'none';

    for (var i = array.length; i--; ) {
        container.appendChild(f(render(array[i])));
    }
    return container;
}

function flow(dir,elist) {
    var array = fromList(elist);
    switch(dir.ctor) {
    case "DDown":  array.reverse();
    case "DUp":    return flowWith(goDown,array);
    case "DRight": array.reverse();
    case "DLeft":  return flowWith(goRight,array);
    case "DOut":   array.reverse();
    case "DIn":    return flowWith(goIn,array);
    }
}

function toPos(pos) {
    switch(pos.ctor) {
    case "Absolute": return  pos._0 + "px";
    case "Relative": return (pos._0 * 100) + "%";
    }
}

// must clear right, left, top, bottom, and transform
// before calling this function
function setPos(pos,elem,e) {
    var element = elem.element;
    var props = elem.props;
    var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
    var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

    e.style.position = 'absolute';
    e.style.margin = 'auto';
    var transform = '';
    switch(pos.horizontal.ctor) {
    case 'P': e.style.right = toPos(pos.x); e.style.removeProperty('left'); break;
    case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
    case 'N': e.style.left = toPos(pos.x); e.style.removeProperty('right'); break;
    }
    switch(pos.vertical.ctor) {
    case 'N': e.style.bottom = toPos(pos.y); e.style.removeProperty('top'); break;
    case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
    case 'P': e.style.top = toPos(pos.y); e.style.removeProperty('bottom'); break;
    }
    if (transform !== '') addTransform(e.style, transform);
    return e;
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos, elem, e);
    var div = newElement('div');
    div.style.position = 'relative';
    div.style.overflow = 'hidden';
    div.appendChild(e);
    return div;
}

function rawHtml(elem) {
    var html = elem.html;
    var args = elem.args;
    var guid = elem.guid;
    var align = elem.align;

    var div = newElement('div');
    div.innerHTML = html;
    div.style.visibility = "hidden";
    if (align) div.style.textAlign = align;
    document.body.appendChild(div);

    for (var i = args.length; i--; ) {
        var arg = args[i];
        var span = document.getElementById('md-' + guid + '-' + i);
        if (arg.isText) {
            span.innerHTML = arg;
        } else {
            span.style.display = 'block';
            span.style.width = arg.props.width + 'px';
            span.style.height = arg.props.height + 'px';
            span.appendChild(render(arg));
        }
    }
    document.body.removeChild(div);
    div.style.visibility = 'visible';
    return div;
}

function render(elem) { return setProps(elem, makeElement(elem)); }
function makeElement(e) {
    var elem = e.element;
    switch(elem.ctor) {
    case 'Image':     return image(e.props, elem);
    case 'Flow':      return flow(elem._0, elem._1);
    case 'Container': return container(elem._0, elem._1);
    case 'Spacer':    return newElement('div');
    case 'RawHtml':   return rawHtml(elem);
    case 'Custom':    return elem.render(elem.model);
    }
}

function update(node, curr, next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr.props.id === next.props.id) return updateProps(node, curr, next);
    if (curr.element.ctor !== next.element.ctor) {
        node.parentNode.replaceChild(render(next),node);
        return true;
    }
    var nextE = next.element, currE = curr.element;
    switch(nextE.ctor) {
    case "Spacer": break;
    case "RawHtml":
        // only markdown blocks have guids, so this must be a text block
        if (nextE.guid === null) {
            if(currE.html.valueOf() !== nextE.html.valueOf()) {
                node.innerHTML = nextE.html;
            }
            break;
        }
        if (nextE.guid !== currE.guid) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nargs = nextE.args;
        var cargs = currE.args;
        for (var i = nargs.length; i--; ) {
            var narg = nargs[i];
            var carg = cargs[i]
            if (narg == carg) continue;
            var span = document.getElementById('md-' + currE.guid + '-' + i);
            if (narg.isElement) {
                if (carg.isElement) {
                    update(span, carg, narg);
                } else {
                    span.style.display = 'block';
                    var e = render(narg);
                    span.innerHTML = '';
                    span.appendChild(e);
                }
            } else {
                span.style.display = 'inline';
                span.innerHTML = narg;
            }
        }
        break;
    case "Image":
        if (nextE._0.ctor === 'Plain') {
            if (nextE._3 !== currE._3) node.src = nextE._3;
        } else if (!eq(nextE,currE) ||
                   next.props.width !== curr.props.width ||
                   next.props.height !== curr.props.height) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        break;
    case "Flow":
        var arr = fromList(nextE._1);
        for (var i = arr.length; i--; ) { arr[i] = arr[i].element.ctor; }
        if (nextE._0.ctor !== currE._0.ctor) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nexts = fromList(nextE._1);
        var kids = node.childNodes;
        if (nexts.length !== kids.length) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var currs = fromList(currE._1);
        var goDir = function(x) { return x; };
        switch(nextE._0.ctor) {
        case "DDown":  case "DUp":   goDir = goDown; break;
        case "DRight": case "DLeft": goDir = goRight; break;
        case "DOut":   case "DIn":   goDir = goIn; break;
        }
        for (var i = kids.length; i-- ;) {
            update(kids[i],currs[i],nexts[i]);
            goDir(kids[i]);
        }
        break;
    case "Container":
        update(node.firstChild, currE._1, nextE._1);
        setPos(nextE._0, nextE._1, node.firstChild);
        break;
    case "Custom":
        if (currE.type === nextE.type) {
            var done = nextE.update(node, currE.model, nextE.model);
            if (done) return;
        } else {
            return node.parentNode.replaceChild(render(next), node);
        }
    }
    updateProps(node, curr, next);
}

function updateProps(node, curr, next) {
    var props = next.props;
    var currP = curr.props;
    var e = node;
    var element = next.element;
    var width = props.width - (element.adjustWidth || 0);
    var height = props.height - (element.adjustHeight || 0);
    if (width !== currP.width) {
        e.style.width = (width|0) + 'px';
    }
    if (height !== currP.height) {
        e.style.height = (height|0) + 'px';
    }
    if (props.opacity !== currP.opacity) {
        e.style.opacity = props.opacity;
    }
    var nextColor = (props.color.ctor === 'Just' ?
                     extract(props.color._0) : '');
    if (e.style.backgroundColor !== nextColor) {
        e.style.backgroundColor = (nextColor === '' ? 'transparent' : nextColor);
    }
    if (props.tag !== currP.tag) { e.id = props.tag; }
    if (props.href !== currP.href) {
        if (currP.href === '') {
            var a = newElement('a');
            a.href = props.href;
            a.style.width = '100%';
            a.style.height = '100%';
            a.style.top = 0;
            a.style.left = 0;
            a.style.display = 'block';
            a.style.position = 'absolute';
            e.style.position = 'relative';
            e.appendChild(a);
        } else {
            node.lastNode.href = props.href;
        }
    }

    // update hover handlers
    if (props.hover.ctor === '_Tuple0') {
        removeHover(e);
    } else if (e.elm_hover_handler) {
        e.elm_hover_handler = props.hover;
    } else {
        addHover(e, props.hover);
    }

    // update click handlers
    if (props.click.ctor === '_Tuple0') {
        removeClick(e);
    } else if (e.elm_click_handler) {
        e.elm_click_handler = props.click;
    } else {
        addClick(e, props.click);
    }
}

return { render:render, update:update };

};
ElmRuntime.Render.Utils = function() {
  function newElement(elementType) {
      var e = document.createElement(elementType);
      e.style.padding = "0";
      e.style.margin = "0";
      return e;
  }

  function addTo(container, elem) {
      container.appendChild(elem);
  }

  function extract(c) {
      if (c._3 === 1) { return 'rgb(' + c._0 + ', ' + c._1 + ', ' + c._2 + ')'; }
      return 'rgba(' + c._0 + ', ' + c._1 + ', ' + c._2 + ', ' + c._3 + ')';
  }

  function addTransform(style, trans) {
    style.transform       = trans;
    style.msTransform     = trans;
    style.MozTransform    = trans;
    style.webkitTransform = trans;
    style.OTransform      = trans;
  }

  function removeTransform(style) {
    style.transform       = 'none';
    style.msTransform     = 'none';
    style.MozTransform    = 'none';
    style.webkitTransform = 'none';
    style.OTransform      = 'none';
  }

  var List = Elm.Native.List.make({});

  return {addTo:addTo,
          newElement:newElement,
          extract : extract,
          fromList: List.toArray,
          fromString: function(s) { return s; },
          toString: function(s) { return s; },
          eq: Elm.Native.Utils.make({}).eq,
          addTransform: addTransform,
          removeTransform: removeTransform};
};

ElmRuntime.Render.Collage = function() {

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Transform = Elm.Transform2D.make({});
var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement,
    extract = Utils.extract, fromList = Utils.fromList,
    fromString = Utils.fromString, addTransform = Utils.addTransform;

function trace(ctx, path) {
    var points = fromList(path);
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i]._0, points[i]._1);
    while (i--) { ctx.lineTo(points[i]._0, points[i]._1); }
    if (path.closed) {
        i = points.length - 1;
        ctx.lineTo(points[i]._0, points[i]._1);
    }
}

function line(ctx,style,path) {
    style.dashing.ctor === 'Nil' ? trace(ctx, path) : customLineHelp(ctx, style, path);
    ctx.scale(1,-1);
    ctx.stroke();
}

function customLineHelp(ctx, style, path) {
    var points = fromList(path);
    if (path.closed) points.push(points[0]);
    var pattern = fromList(style.dashing);
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i]._0, y0 = points[i]._1;
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
        x1 = points[i]._0; y1 = points[i]._1;
        dx = x1 - x0; dy = y1 - y0;
        remaining = Math.sqrt(dx * dx + dy * dy);
        while (segmentLength <= remaining) {
            x0 += dx * segmentLength / remaining;
            y0 += dy * segmentLength / remaining;
            ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
            // update starting position
            dx = x1 - x0; dy = y1 - y0;
            remaining = Math.sqrt(dx * dx + dy * dy);
            // update pattern
            draw = !draw;
            pindex = (pindex + 1) % plen;
            segmentLength = pattern[pindex];
        }
        if (remaining > 0) {
            ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
            segmentLength -= remaining;
        }
        x0 = x1; y0 = y1;
    }
}

function drawLine(ctx, style, path) {
    ctx.lineWidth = style.width;
    var cap = style.cap.ctor;
    ctx.lineCap = cap === 'Flat' ? 'butt' :
                  cap === 'Round' ? 'round' : 'square';
    var join = style.join.ctor;
    ctx.lineJoin = join === 'Smooth' ? 'round' :
                   join === 'Sharp' ? 'miter' : 'bevel';
    ctx.miterLimit = style.join._0 || 10;
    ctx.strokeStyle = extract(style.color);
    return line(ctx, style, path);
}

function texture(redo, ctx, src) {
    var img = new Image();
    img.src = fromString(src);
    img.onload = redo;
    return ctx.createPattern(img, 'repeat');
}

function gradient(ctx, grad) {
  var g;
  var stops = [];
  if (grad.ctor === 'Linear') {
    var p0 = grad._0, p1 = grad._1;
    g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
    stops = fromList(grad._2);
  } else {
    var p0 = grad._0, p2 = grad._2;
    g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
    stops = fromList(grad._4);
  }
  var len = stops.length;
  for (var i = 0; i < len; ++i) {
    var stop = stops[i];
    g.addColorStop(stop._0, extract(stop._1));
  }
  return g;
}

function drawShape(redo, ctx, style, path) {
    trace(ctx, path);
    var sty = style.ctor;
    ctx.fillStyle =
        sty === 'Solid' ? extract(style._0) :
        sty === 'Texture' ? texture(redo, ctx, style._0) : gradient(ctx, style._0);
    ctx.scale(1,-1);
    ctx.fill();
}

function drawImage(redo, ctx, form) {
    var img = new Image();
    img.onload = redo;
    img.src = fromString(form._3);
    var w = form._0,
        h = form._1,
        pos = form._2,
        srcX = pos._0,
        srcY = pos._1,
        srcW = w,
        srcH = h,
        destX = -w/2,
        destY = -h/2,
        destW = w,
        destH = h;

    ctx.scale(1,-1);
    ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
}

function renderForm(redo, ctx, form) {
    ctx.save();
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) ctx.translate(x, y);
    if (theta !== 0) ctx.rotate(theta);
    if (scale !== 1) ctx.scale(scale,scale);
    if (form.alpha !== 1) ctx.globalAlpha = ctx.globalAlpha * form.alpha;
    ctx.beginPath();
    var f = form.form;
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FShape':
      if (f._0.ctor === 'Left') {
        f._1.closed = true;
        drawLine(ctx, f._0._0, f._1);
      } else {
        drawShape(redo, ctx, f._0._0, f._1);
      }
    break;
    }
    ctx.restore();
}

function formToMatrix(form) {
   var scale = form.scale;
   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

   var theta = form.theta
   if (theta !== 0)
       matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );

   return matrix;
}

function str(n) {
    if (n < 0.00001 && n > -0.00001) return 0;
    return n;
}

function makeTransform(w, h, form, matrices) {
    var props = form.form._0.props;
    var m = A6( Transform.matrix, 1, 0, 0, -1,
                (w - props.width ) / 2,
                (h - props.height) / 2 );
    var len = matrices.length;
    for (var i = 0; i < len; ++i) { m = A2( Transform.multiply, m, matrices[i] ); }
    m = A2( Transform.multiply, m, formToMatrix(form) );

    return 'matrix(' + str( m[0]) + ', ' + str( m[3]) + ', ' +
                       str(-m[1]) + ', ' + str(-m[4]) + ', ' +
                       str( m[2]) + ', ' + str( m[5]) + ')';
}

function stepperHelp(list) {
    var arr = fromList(list);
    var i = 0;
    function peekNext() {
        return i < arr.length ? arr[i].form.ctor : '';
    }
    // assumes that there is a next element
    function next() {
        var out = arr[i];
        ++i;
        return out;
    }
    return { peekNext:peekNext, next:next };
}

function formStepper(forms) {
    var ps = [stepperHelp(forms)];
    var matrices = [];
    var alphas = [];
    function peekNext() {
        var len = ps.length;
        var formType = '';
        for (var i = 0; i < len; ++i ) {
            if (formType = ps[i].peekNext()) return formType;
        }
        return '';
    }
    // assumes that there is a next element
    function next(ctx) {
        while (!ps[0].peekNext()) {
            ps.shift();
            matrices.pop();
            alphas.shift();
            if (ctx) { ctx.restore(); }
        }
        var out = ps[0].next();
        var f = out.form;
        if (f.ctor === 'FGroup') {
            ps.unshift(stepperHelp(f._1));
            var m = A2(Transform.multiply, f._0, formToMatrix(out));
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
            matrices.push(m);

            var alpha = (alphas[0] || 1) * out.alpha;
            alphas.unshift(alpha);
            ctx.globalAlpha = alpha;
        }
        return out;
    }
    function transforms() { return matrices; }
    function alpha() { return alphas[0] || 1; }
    return { peekNext:peekNext, next:next, transforms:transforms, alpha:alpha };
}

function makeCanvas(w,h) {
    var canvas = newElement('canvas');
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.style.position = "absolute";
    canvas.width  = w;
    canvas.height = h;
    return canvas;
}

function render(model) {
    var div = newElement('div');
    div.style.overflow = 'hidden';
    div.style.position = 'relative';
    update(div, model, model);
    return div;
}

function nodeStepper(w,h,div) {
    var kids = div.childNodes;
    var i = 0;
    function transform(transforms, ctx) {
        ctx.translate(w/2, h/2);
        ctx.scale(1,-1);
        var len = transforms.length;
        for (var i = 0; i < len; ++i) {
            var m = transforms[i];
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
        }
        return ctx;
    }
    function nextContext(transforms) {
        while (i < kids.length) {
            var node = kids[i];
            if (node.getContext) {
                node.width = w;
                node.height = h;
                node.style.width = w + 'px';
                node.style.height = h + 'px';
                ++i;
                return transform(transforms, node.getContext('2d'));
            }
            div.removeChild(node);
        }
        var canvas = makeCanvas(w,h);
        div.appendChild(canvas);
        // we have added a new node, so we must step our position
        ++i;
        return transform(transforms, canvas.getContext('2d'));
    }
    function addElement(matrices, alpha, form) {
        var kid = kids[i];
        var elem = form.form._0;

        var node = (!kid || kid.getContext)
            ? Render.render(elem)
            : (Render.update(kid, kid.oldElement, elem), kids[i]);

        node.style.position = 'absolute';
        node.style.opacity = alpha * form.alpha;
        addTransform(node.style, makeTransform(w, h, form, matrices));
        node.oldElement = elem;
        ++i;
        if (!kid) {
            div.appendChild(node);
        } else if (kid.getContext) {
            div.insertBefore(node, kid);
        }
    }
    function clearRest() {
        while (i < kids.length) {
            div.removeChild(kids[i]);
        }
    }
    return { nextContext:nextContext, addElement:addElement, clearRest:clearRest };
}


function update(div, _, model) {
    var w = model.w;
    var h = model.h;

    var forms = formStepper(model.forms);
    var nodes = nodeStepper(w,h,div);
    var ctx = null;
    var formType = '';

    while (formType = forms.peekNext()) {
        // make sure we have context if we need it
        if (ctx === null && formType !== 'FElement') {
            ctx = nodes.nextContext(forms.transforms());
            ctx.globalAlpha = forms.alpha();
        }

        var form = forms.next(ctx);
        // if it is FGroup, all updates are made within formStepper when next is called.
        if (formType === 'FElement') {
            // update or insert an element, get a new context
            nodes.addElement(forms.transforms(), forms.alpha(), form);
            ctx = null;
        } else if (formType !== 'FGroup') {
            renderForm(function() { update(div, model, model); }, ctx, form);
        }
    }
    nodes.clearRest();
    return false;
}

return { render:render, update:update };

};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Main";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Assertion = Elm.ElmTest.Assertion.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Run = Elm.ElmTest.Run.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Runner = ElmTest.Runner || {};
   ElmTest.Runner.Console = Elm.ElmTest.Runner.Console.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Test = Elm.ElmTest.Test.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var IO = IO || {};
   IO.IO = Elm.IO.IO.make(_elm);
   var IO = IO || {};
   IO.Runner = Elm.IO.Runner.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var responses = Native.Ports.portIn("responses",
   Native.Ports.incomingSignal(function (v) {
      return v === null ? Maybe.Nothing : Maybe.Just(typeof v === "string" || typeof v === "object" && v instanceof String ? v : _E.raise("invalid input, expecting JSString but got " + v));
   }));
   var tests = _L.fromArray([A2(ElmTest.Test.equals,
   0,
   1)]);
   var console = ElmTest.Runner.Console.runDisplay(tests);
   var requests = Native.Ports.portOut("requests",
   Native.Ports.outgoingSignal(function (v) {
      return _L.toArray(v).map(function (v) {
         return {mPut: v.mPut.ctor === "Nothing" ? null : v.mPut._0
                ,mExit: v.mExit.ctor === "Nothing" ? null : v.mExit._0
                ,mGet: v.mGet};
      });
   }),
   A2(IO.Runner.run,
   responses,
   console));
   _elm.Main.values = {_op: _op
                      ,tests: tests
                      ,console: console};
   return _elm.Main.values;
};Elm.IO = Elm.IO || {};
Elm.IO.Runner = Elm.IO.Runner || {};
Elm.IO.Runner.make = function (_elm) {
   "use strict";
   _elm.IO = _elm.IO || {};
   _elm.IO.Runner = _elm.IO.Runner || {};
   if (_elm.IO.Runner.values)
   return _elm.IO.Runner.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "IO.Runner";
   var Automaton = Elm.Automaton.make(_elm);
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Either = Elm.Either.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var IO = IO || {};
   IO.IO = Elm.IO.IO.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var put = F2(function (s,_v0) {
      return function () {
         return {ctor: "_Tuple2"
                ,_0: s
                ,_1: {ctor: "_Tuple0"}};
      }();
   });
   var ask = function (s) {
      return {ctor: "_Tuple2"
             ,_0: s
             ,_1: s};
   };
   _op[">>="] = F3(function (f,
   k,
   s) {
      return function () {
         var $ = f(s),
         s$ = $._0,
         y = $._1;
         return A2(k,y,s$);
      }();
   });
   var pure = F2(function (x,s) {
      return {ctor: "_Tuple2"
             ,_0: s
             ,_1: x};
   });
   var empty = {_: {}
               ,mExit: Maybe.Nothing
               ,mGet: false
               ,mPut: Maybe.Nothing};
   var putS = function (s) {
      return _U.replace([["mPut"
                         ,Maybe.Just(s)]],
      empty);
   };
   var exit = function (n) {
      return _U.replace([["mExit"
                         ,Maybe.Just(n)]],
      empty);
   };
   var getS = _U.replace([["mGet"
                          ,true]],
   empty);
   var extractRequests = function (io) {
      return function () {
         switch (io.ctor)
         {case "Impure":
            return function () {
                 switch (io._0.ctor)
                 {case "Exit":
                    return pure({ctor: "_Tuple2"
                                ,_0: _L.fromArray([exit(io._0._0)])
                                ,_1: io});
                    case "GetC":
                    return A2(_op[">>="],
                      ask,
                      function (st) {
                         return function () {
                            var _v10 = String.uncons(st.buffer);
                            switch (_v10.ctor)
                            {case "Just":
                               switch (_v10._0.ctor)
                                 {case "_Tuple2":
                                    return A2(_op[">>="],
                                      put({_: {},buffer: _v10._0._1}),
                                      function (_v14) {
                                         return function () {
                                            return extractRequests(io._0._0(_v10._0._0));
                                         }();
                                      });}
                                 break;
                               case "Nothing":
                               return pure({ctor: "_Tuple2"
                                           ,_0: _L.fromArray([getS])
                                           ,_1: io});}
                            _E.Case($moduleName,
                            "between lines 51 and 55");
                         }();
                      });
                    case "PutC":
                    return A2(_op[">>="],
                      extractRequests(io._0._1),
                      function (_v16) {
                         return function () {
                            switch (_v16.ctor)
                            {case "_Tuple2":
                               return pure({ctor: "_Tuple2"
                                           ,_0: {ctor: "::"
                                                ,_0: putS(A2(String.cons,
                                                io._0._0,
                                                ""))
                                                ,_1: _v16._0}
                                           ,_1: _v16._1});}
                            _E.Case($moduleName,
                            "on line 47, column 22 to 61");
                         }();
                      });}
                 _E.Case($moduleName,
                 "between lines 45 and 55");
              }();
            case "Pure":
            return pure({ctor: "_Tuple2"
                        ,_0: _L.fromArray([exit(0)])
                        ,_1: IO.IO.Pure(io._0)});}
         _E.Case($moduleName,
         "between lines 43 and 55");
      }();
   };
   var step = F2(function (resp,
   _v20) {
      return function () {
         switch (_v20.ctor)
         {case "_Tuple2":
            return function () {
                 var newST = function () {
                    switch (resp.ctor)
                    {case "Just":
                       return _U.replace([["buffer"
                                          ,A2(String.append,
                                          _v20._1.buffer,
                                          resp._0)]],
                         _v20._1);
                       case "Nothing": return _v20._1;}
                    _E.Case($moduleName,
                    "between lines 59 and 62");
                 }();
                 var _ = A2(extractRequests,
                 _v20._0,
                 newST);
                 var k = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2":
                            return _._1._1;}
                         break;}
                    _E.Case($moduleName,
                    "on line 62, column 27 to 51");
                 }();
                 var newST$ = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2": return _._0;}
                         break;}
                    _E.Case($moduleName,
                    "on line 62, column 27 to 51");
                 }();
                 var rs = function () {
                    switch (_.ctor)
                    {case "_Tuple2":
                       switch (_._1.ctor)
                         {case "_Tuple2":
                            return _._1._0;}
                         break;}
                    _E.Case($moduleName,
                    "on line 62, column 27 to 51");
                 }();
                 return {ctor: "_Tuple2"
                        ,_0: {ctor: "_Tuple2"
                             ,_0: k
                             ,_1: newST$}
                        ,_1: rs};
              }();}
         _E.Case($moduleName,
         "between lines 59 and 63");
      }();
   });
   var start = {_: {},buffer: ""};
   var run = F2(function (resps,
   io) {
      return A3(Automaton.run,
      A2(Automaton.hiddenState,
      {ctor: "_Tuple2"
      ,_0: io
      ,_1: start},
      step),
      _L.fromArray([]),
      resps);
   });
   var orSig = F2(function (s1,
   s2) {
      return A2(Signal.merge,
      A2(Signal._op["<~"],
      Either.Left,
      s1),
      A2(Signal._op["<~"],
      Either.Right,
      s2));
   });
   var IOState = function (a) {
      return {_: {},buffer: a};
   };
   var Request = F3(function (a,
   b,
   c) {
      return {_: {}
             ,mExit: b
             ,mGet: c
             ,mPut: a};
   });
   _elm.IO.Runner.values = {_op: _op
                           ,orSig: orSig
                           ,start: start
                           ,run: run
                           ,empty: empty
                           ,putS: putS
                           ,exit: exit
                           ,getS: getS
                           ,extractRequests: extractRequests
                           ,step: step
                           ,pure: pure
                           ,ask: ask
                           ,put: put
                           ,Request: Request
                           ,IOState: IOState};
   return _elm.IO.Runner.values;
};Elm.Automaton = Elm.Automaton || {};
Elm.Automaton.make = function (_elm) {
   "use strict";
   _elm.Automaton = _elm.Automaton || {};
   if (_elm.Automaton.values)
   return _elm.Automaton.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Automaton";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var dequeue = function (q) {
      return function () {
         switch (q.ctor)
         {case "_Tuple2":
            switch (q._0.ctor)
              {case "[]": switch (q._1.ctor)
                   {case "[]":
                      return Maybe.Nothing;}
                   break;}
              switch (q._1.ctor)
              {case "::":
                 return Maybe.Just({ctor: "_Tuple2"
                                   ,_0: q._1._0
                                   ,_1: {ctor: "_Tuple2"
                                        ,_0: q._0
                                        ,_1: q._1._1}});
                 case "[]":
                 return dequeue({ctor: "_Tuple2"
                                ,_0: _L.fromArray([])
                                ,_1: List.reverse(q._0)});}
              break;}
         _E.Case($moduleName,
         "between lines 116 and 121");
      }();
   };
   var enqueue = F2(function (x,
   _v5) {
      return function () {
         switch (_v5.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: {ctor: "::"
                        ,_0: x
                        ,_1: _v5._0}
                   ,_1: _v5._1};}
         _E.Case($moduleName,
         "on line 115, column 22 to 31");
      }();
   });
   var empty = {ctor: "_Tuple2"
               ,_0: _L.fromArray([])
               ,_1: _L.fromArray([])};
   var Step = function (a) {
      return {ctor: "Step",_0: a};
   };
   var run = F3(function (auto,
   base,
   inputs) {
      return function () {
         var step = F2(function (a,
         _v9) {
            return function () {
               switch (_v9.ctor)
               {case "_Tuple2":
                  switch (_v9._0.ctor)
                    {case "Step":
                       return _v9._0._0(a);}
                    break;}
               _E.Case($moduleName,
               "on line 43, column 28 to 31");
            }();
         });
         return A2(Signal.lift,
         function (_v14) {
            return function () {
               switch (_v14.ctor)
               {case "_Tuple2":
                  return _v14._1;}
               _E.Case($moduleName,
               "on line 44, column 23 to 24");
            }();
         },
         A3(Signal.foldp,
         step,
         {ctor: "_Tuple2"
         ,_0: auto
         ,_1: base},
         inputs));
      }();
   });
   var step = F2(function (a,
   _v18) {
      return function () {
         switch (_v18.ctor)
         {case "Step":
            return _v18._0(a);}
         _E.Case($moduleName,
         "on line 48, column 19 to 22");
      }();
   });
   var andThen = F2(function (f,
   g) {
      return Step(function (a) {
         return function () {
            var $ = A2(step,a,f),
            f$ = $._0,
            b = $._1;
            var $ = A2(step,b,g),
            g$ = $._0,
            c = $._1;
            return {ctor: "_Tuple2"
                   ,_0: A2(andThen,f$,g$)
                   ,_1: c};
         }();
      });
   });
   var loop = F2(function (state,
   auto) {
      return Step(function (input) {
         return function () {
            var _ = A2(step,
            {ctor: "_Tuple2"
            ,_0: input
            ,_1: state},
            auto);
            var auto$ = function () {
               switch (_.ctor)
               {case "_Tuple2":
                  switch (_._1.ctor)
                    {case "_Tuple2": return _._0;}
                    break;}
               _E.Case($moduleName,
               "on line 68, column 54 to 77");
            }();
            var output = function () {
               switch (_.ctor)
               {case "_Tuple2":
                  switch (_._1.ctor)
                    {case "_Tuple2":
                       return _._1._0;}
                    break;}
               _E.Case($moduleName,
               "on line 68, column 54 to 77");
            }();
            var state$ = function () {
               switch (_.ctor)
               {case "_Tuple2":
                  switch (_._1.ctor)
                    {case "_Tuple2":
                       return _._1._1;}
                    break;}
               _E.Case($moduleName,
               "on line 68, column 54 to 77");
            }();
            return {ctor: "_Tuple2"
                   ,_0: A2(loop,state$,auto$)
                   ,_1: output};
         }();
      });
   });
   var combine = function (autos) {
      return Step(function (a) {
         return function () {
            var $ = List.unzip(A2(List.map,
            step(a),
            autos)),
            autos$ = $._0,
            bs = $._1;
            return {ctor: "_Tuple2"
                   ,_0: combine(autos$)
                   ,_1: bs};
         }();
      });
   };
   var pure = function (f) {
      return Step(function (x) {
         return {ctor: "_Tuple2"
                ,_0: pure(f)
                ,_1: f(x)};
      });
   };
   var state = F2(function (s,f) {
      return Step(function (x) {
         return function () {
            var s$ = A2(f,x,s);
            return {ctor: "_Tuple2"
                   ,_0: A2(state,s$,f)
                   ,_1: s$};
         }();
      });
   });
   var count = A2(state,
   0,
   F2(function (_v36,c) {
      return function () {
         return c + 1;
      }();
   }));
   var hiddenState = F2(function (s,
   f) {
      return Step(function (x) {
         return function () {
            var $ = A2(f,x,s),
            s$ = $._0,
            out = $._1;
            return {ctor: "_Tuple2"
                   ,_0: A2(hiddenState,s$,f)
                   ,_1: out};
         }();
      });
   });
   var average = function (k) {
      return function () {
         var stepFull = F2(function (n,
         _v38) {
            return function () {
               switch (_v38.ctor)
               {case "_Tuple3":
                  return function () {
                       var _v43 = dequeue(_v38._0);
                       switch (_v43.ctor)
                       {case "Just":
                          switch (_v43._0.ctor)
                            {case "_Tuple2":
                               return function () {
                                    var sum$ = _v38._2 + n - _v43._0._0;
                                    return {ctor: "_Tuple2"
                                           ,_0: {ctor: "_Tuple3"
                                                ,_0: A2(enqueue,n,_v43._0._1)
                                                ,_1: _v38._1
                                                ,_2: sum$}
                                           ,_1: sum$ / Basics.toFloat(_v38._1)};
                                 }();}
                            break;
                          case "Nothing":
                          return {ctor: "_Tuple2"
                                 ,_0: {ctor: "_Tuple3"
                                      ,_0: _v38._0
                                      ,_1: _v38._1
                                      ,_2: _v38._2}
                                 ,_1: 0};}
                       _E.Case($moduleName,
                       "between lines 128 and 132");
                    }();}
               _E.Case($moduleName,
               "between lines 128 and 132");
            }();
         });
         var step = F2(function (n,
         _v47) {
            return function () {
               switch (_v47.ctor)
               {case "_Tuple3":
                  return _U.eq(_v47._1,
                    k) ? A2(stepFull,
                    n,
                    {ctor: "_Tuple3"
                    ,_0: _v47._0
                    ,_1: _v47._1
                    ,_2: _v47._2}) : {ctor: "_Tuple2"
                                     ,_0: {ctor: "_Tuple3"
                                          ,_0: A2(enqueue,n,_v47._0)
                                          ,_1: _v47._1 + 1
                                          ,_2: _v47._2 + n}
                                     ,_1: (_v47._2 + n) / (Basics.toFloat(_v47._1) + 1)};}
               _E.Case($moduleName,
               "between lines 125 and 126");
            }();
         });
         return A2(hiddenState,
         {ctor: "_Tuple3"
         ,_0: empty
         ,_1: 0
         ,_2: 0},
         step);
      }();
   };
   _elm.Automaton.values = {_op: _op
                           ,pure: pure
                           ,state: state
                           ,hiddenState: hiddenState
                           ,run: run
                           ,step: step
                           ,andThen: andThen
                           ,combine: combine
                           ,loop: loop
                           ,count: count
                           ,average: average};
   return _elm.Automaton.values;
};Elm.ElmTest = Elm.ElmTest || {};
Elm.ElmTest.Runner = Elm.ElmTest.Runner || {};
Elm.ElmTest.Runner.Console = Elm.ElmTest.Runner.Console || {};
Elm.ElmTest.Runner.Console.make = function (_elm) {
   "use strict";
   _elm.ElmTest = _elm.ElmTest || {};
   _elm.ElmTest.Runner = _elm.ElmTest.Runner || {};
   _elm.ElmTest.Runner.Console = _elm.ElmTest.Runner.Console || {};
   if (_elm.ElmTest.Runner.Console.values)
   return _elm.ElmTest.Runner.Console.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "ElmTest.Runner.Console";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Runner = ElmTest.Runner || {};
   ElmTest.Runner.String = Elm.ElmTest.Runner.String.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Test = Elm.ElmTest.Test.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var IO = IO || {};
   IO.IO = Elm.IO.IO.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var runDisplay = function (tests) {
      return function () {
         var $ = ElmTest.Runner.String.runDisplay(tests),
         allPassed = $._0,
         results = $._1;
         return A2(IO.IO._op[">>"],
         IO.IO.putStrLn(results),
         function () {
            switch (allPassed)
            {case false:
               return IO.IO.exit(1);
               case true:
               return IO.IO.exit(0);}
            _E.Case($moduleName,
            "between lines 23 and 25");
         }());
      }();
   };
   _elm.ElmTest.Runner.Console.values = {_op: _op
                                        ,runDisplay: runDisplay};
   return _elm.ElmTest.Runner.Console.values;
};Elm.ElmTest = Elm.ElmTest || {};
Elm.ElmTest.Runner = Elm.ElmTest.Runner || {};
Elm.ElmTest.Runner.String = Elm.ElmTest.Runner.String || {};
Elm.ElmTest.Runner.String.make = function (_elm) {
   "use strict";
   _elm.ElmTest = _elm.ElmTest || {};
   _elm.ElmTest.Runner = _elm.ElmTest.Runner || {};
   _elm.ElmTest.Runner.String = _elm.ElmTest.Runner.String || {};
   if (_elm.ElmTest.Runner.String.values)
   return _elm.ElmTest.Runner.String.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "ElmTest.Runner.String";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Run = Elm.ElmTest.Run.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Test = Elm.ElmTest.Test.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var pretty = F2(function (_v0,
   r) {
      return function () {
         switch (_v0.ctor)
         {case "TestCase":
            return function () {
                 switch (r.ctor)
                 {case "Just":
                    return _L.append(_v0._0,
                      _L.append(": FAILED. ",r._0));
                    case "Nothing":
                    return _L.append(_v0._0,
                      ": passed.");}
                 _E.Case($moduleName,
                 "between lines 29 and 33");
              }();}
         _E.Case($moduleName,
         "between lines 29 and 33");
      }();
   });
   var replicate = F2(function (n,
   c) {
      return function () {
         var go = function (n) {
            return _U.cmp(n,
            0) < 1 ? _L.fromArray([]) : {ctor: "::"
                                        ,_0: c
                                        ,_1: go(n - 1)};
         };
         return String.fromList(go(n));
      }();
   });
   var vcat = function ($) {
      return List.concat(List.intersperse("\n")($));
   };
   var indent = function (n) {
      return function () {
         var indents = A2(replicate,
         n,
         _U.chr(" "));
         return function ($) {
            return vcat(List.map(String.append(indents))(String.lines($)));
         };
      }();
   };
   var runDisplay = function (ts) {
      return function () {
         var r = ElmTest.Run.report(ts);
         var passed = List.length(r.passes);
         var failed = List.length(r.failures);
         var summary = vcat(List.map(indent(2))(_L.fromArray([_L.append(String.show(List.length(ts)),
                                                             " tests executed")
                                                             ,_L.append(String.show(passed),
                                                             " tests passed")
                                                             ,_L.append(String.show(failed),
                                                             " tests failed")])));
         var pass = _U.eq(failed,0);
         var results = pass ? _L.fromArray([]) : A3(List.zipWith,
         pretty,
         ts,
         r.results);
         return {ctor: "_Tuple2"
                ,_0: pass
                ,_1: vcat({ctor: "::"
                          ,_0: summary
                          ,_1: results})};
      }();
   };
   _elm.ElmTest.Runner.String.values = {_op: _op
                                       ,runDisplay: runDisplay};
   return _elm.ElmTest.Runner.String.values;
};Elm.ElmTest = Elm.ElmTest || {};
Elm.ElmTest.Run = Elm.ElmTest.Run || {};
Elm.ElmTest.Run.make = function (_elm) {
   "use strict";
   _elm.ElmTest = _elm.ElmTest || {};
   _elm.ElmTest.Run = _elm.ElmTest.Run || {};
   if (_elm.ElmTest.Run.values)
   return _elm.ElmTest.Run.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "ElmTest.Run";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Assertion = Elm.ElmTest.Assertion.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Test = Elm.ElmTest.Test.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var pass = function (m) {
      return function () {
         switch (m.ctor)
         {case "Just": return false;
            case "Nothing": return true;}
         _E.Case($moduleName,
         "between lines 33 and 37");
      }();
   };
   var fail = function ($) {
      return Basics.not(pass($));
   };
   var run = function (_v2) {
      return function () {
         switch (_v2.ctor)
         {case "TestCase":
            return function () {
                 var runAssertion = F2(function (t,
                 m) {
                    return t({ctor: "_Tuple0"}) ? Maybe.Nothing : Maybe.Just(m);
                 });
                 return function () {
                    switch (_v2._1.ctor)
                    {case "AssertEqual":
                       return runAssertion(_v2._1._0)(_L.append("Expected: ",
                         _L.append(_v2._1._1,
                         _L.append("; got: ",
                         _v2._1._2))));
                       case "AssertFalse":
                       return runAssertion(_v2._1._0)("not False");
                       case "AssertNotEqual":
                       return runAssertion(_v2._1._0)(_L.append(_v2._1._1,
                         _L.append(" equals ",
                         _v2._1._2)));
                       case "AssertTrue":
                       return runAssertion(_v2._1._0)("not True");}
                    _E.Case($moduleName,
                    "between lines 25 and 31");
                 }();
              }();}
         _E.Case($moduleName,
         "between lines 22 and 31");
      }();
   };
   var report = function (ts) {
      return function () {
         var results = A2(List.map,
         run,
         ts);
         var $ = A2(List.partition,
         pass,
         results),
         passes = $._0,
         fails = $._1;
         return {_: {}
                ,failures: fails
                ,passes: passes
                ,results: results};
      }();
   };
   var Report = F3(function (a,
   b,
   c) {
      return {_: {}
             ,failures: c
             ,passes: b
             ,results: a};
   });
   _elm.ElmTest.Run.values = {_op: _op
                             ,run: run
                             ,pass: pass
                             ,fail: fail
                             ,report: report
                             ,Report: Report};
   return _elm.ElmTest.Run.values;
};Elm.ElmTest = Elm.ElmTest || {};
Elm.ElmTest.Test = Elm.ElmTest.Test || {};
Elm.ElmTest.Test.make = function (_elm) {
   "use strict";
   _elm.ElmTest = _elm.ElmTest || {};
   _elm.ElmTest.Test = _elm.ElmTest.Test || {};
   if (_elm.ElmTest.Test.values)
   return _elm.ElmTest.Test.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "ElmTest.Test";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var ElmTest = ElmTest || {};
   ElmTest.Assertion = Elm.ElmTest.Assertion.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var TestCase = F2(function (a,
   b) {
      return {ctor: "TestCase"
             ,_0: a
             ,_1: b};
   });
   var test = F2(function (name,
   a) {
      return A2(TestCase,name,a);
   });
   var defaultTest = function (a) {
      return function () {
         var name = function () {
            switch (a.ctor)
            {case "AssertEqual":
               return _L.append(a._1,
                 _L.append(" == ",a._2));
               case "AssertNotEqual":
               return _L.append(a._1,
                 _L.append(" /= ",a._2));
               case "AssertTrue":
               return "True";}
            _E.Case($moduleName,
            "between lines 25 and 30");
         }();
         return A2(test,name,a);
      }();
   };
   var equals = F2(function (a,b) {
      return defaultTest(A2(ElmTest.Assertion.assertEqual,
      a,
      b));
   });
   _elm.ElmTest.Test.values = {_op: _op
                              ,equals: equals
                              ,test: test
                              ,defaultTest: defaultTest
                              ,TestCase: TestCase};
   return _elm.ElmTest.Test.values;
};Elm.ElmTest = Elm.ElmTest || {};
Elm.ElmTest.Assertion = Elm.ElmTest.Assertion || {};
Elm.ElmTest.Assertion.make = function (_elm) {
   "use strict";
   _elm.ElmTest = _elm.ElmTest || {};
   _elm.ElmTest.Assertion = _elm.ElmTest.Assertion || {};
   if (_elm.ElmTest.Assertion.values)
   return _elm.ElmTest.Assertion.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "ElmTest.Assertion";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var AssertNotEqual = F3(function (a,
   b,
   c) {
      return {ctor: "AssertNotEqual"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var assertNotEqual = F2(function (a,
   b) {
      return A3(AssertNotEqual,
      function (_v0) {
         return function () {
            return !_U.eq(a,b);
         }();
      },
      String.show(a),
      String.show(b));
   });
   var AssertEqual = F3(function (a,
   b,
   c) {
      return {ctor: "AssertEqual"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var assertEqual = F2(function (a,
   b) {
      return A3(AssertEqual,
      function (_v2) {
         return function () {
            return _U.eq(a,b);
         }();
      },
      String.show(a),
      String.show(b));
   });
   var assertionList = F2(function (xs,
   ys) {
      return A3(List.zipWith,
      assertEqual,
      xs,
      ys);
   });
   var AssertFalse = function (a) {
      return {ctor: "AssertFalse"
             ,_0: a};
   };
   var AssertTrue = function (a) {
      return {ctor: "AssertTrue"
             ,_0: a};
   };
   var assertT = AssertTrue;
   var assert = function (b) {
      return AssertTrue(function (_v4) {
         return function () {
            return b;
         }();
      });
   };
   _elm.ElmTest.Assertion.values = {_op: _op
                                   ,assertT: assertT
                                   ,assert: assert
                                   ,assertEqual: assertEqual
                                   ,assertionList: assertionList
                                   ,assertNotEqual: assertNotEqual
                                   ,AssertTrue: AssertTrue
                                   ,AssertFalse: AssertFalse
                                   ,AssertEqual: AssertEqual
                                   ,AssertNotEqual: AssertNotEqual};
   return _elm.ElmTest.Assertion.values;
};Elm.IO = Elm.IO || {};
Elm.IO.IO = Elm.IO.IO || {};
Elm.IO.IO.make = function (_elm) {
   "use strict";
   _elm.IO = _elm.IO || {};
   _elm.IO.IO = _elm.IO.IO || {};
   if (_elm.IO.IO.values)
   return _elm.IO.IO.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "IO.IO";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var Impure = function (a) {
      return {ctor: "Impure"
             ,_0: a};
   };
   var Pure = function (a) {
      return {ctor: "Pure",_0: a};
   };
   var Exit = function (a) {
      return {ctor: "Exit",_0: a};
   };
   var GetC = function (a) {
      return {ctor: "GetC",_0: a};
   };
   var PutC = F2(function (a,b) {
      return {ctor: "PutC"
             ,_0: a
             ,_1: b};
   });
   var mapF = F2(function (f,iof) {
      return function () {
         switch (iof.ctor)
         {case "Exit":
            return Exit(iof._0);
            case "GetC":
            return GetC(function ($) {
                 return f(iof._0($));
              });
            case "PutC": return A2(PutC,
              iof._0,
              f(iof._1));}
         _E.Case($moduleName,
         "between lines 77 and 80");
      }();
   });
   var foldIO = F3(function (pur,
   impur,
   io) {
      return function () {
         switch (io.ctor)
         {case "Impure":
            return impur(A2(mapF,
              A2(foldIO,pur,impur),
              io._0));
            case "Pure": return pur(io._0);}
         _E.Case($moduleName,
         "between lines 84 and 86");
      }();
   });
   var bind = F2(function (io,f) {
      return function () {
         switch (io.ctor)
         {case "Impure":
            return Impure(A2(mapF,
              A2(Basics.flip,bind,f),
              io._0));
            case "Pure": return f(io._0);}
         _E.Case($moduleName,
         "between lines 52 and 54");
      }();
   });
   _op[">>="] = bind;
   var seq = F2(function (x,y) {
      return A2(_op[">>="],
      x,
      function (_v11) {
         return function () {
            return y;
         }();
      });
   });
   _op[">>"] = seq;
   var forever = function (m) {
      return A2(_op[">>="],
      m,
      function (_v13) {
         return function () {
            return forever(m);
         }();
      });
   };
   var pure = Pure;
   var apply = F2(function (iof,
   iom) {
      return A2(_op[">>="],
      iof,
      function (f) {
         return A2(_op[">>="],
         iom,
         function (m) {
            return pure(f(m));
         });
      });
   });
   _op["<*>"] = apply;
   var mapIO = F2(function (f,xs) {
      return A3(List.foldr,
      function ($) {
         return F2(function (x,y) {
            return A2(_op[">>"],x,y);
         })(f($));
      },
      pure({ctor: "_Tuple0"}),
      xs);
   });
   var map = F2(function (f,io) {
      return function () {
         switch (io.ctor)
         {case "Impure":
            return Impure(A2(mapF,
              map(f),
              io._0));
            case "Pure":
            return Pure(f(io._0));}
         _E.Case($moduleName,
         "between lines 33 and 35");
      }();
   });
   var exit = function ($) {
      return Impure(Exit($));
   };
   var getChar = Impure(GetC(Pure));
   var readUntil = function (end) {
      return function () {
         var go = function (s) {
            return A2(_op[">>="],
            getChar,
            function (c) {
               return _U.eq(c,
               end) ? pure(s) : go(A2(String.append,
               s,
               A2(String.cons,c,"")));
            });
         };
         return go("");
      }();
   };
   var getLine = readUntil(_U.chr("\n"));
   var putChar = function (c) {
      return Impure(A2(PutC,
      c,
      Pure({ctor: "_Tuple0"})));
   };
   var putStr = function ($) {
      return mapIO(putChar)(String.toList($));
   };
   var putStrLn = function (s) {
      return A2(_op[">>"],
      putStr(s),
      putChar(_U.chr("\n")));
   };
   _elm.IO.IO.values = {_op: _op
                       ,putChar: putChar
                       ,getChar: getChar
                       ,exit: exit
                       ,putStr: putStr
                       ,putStrLn: putStrLn
                       ,readUntil: readUntil
                       ,getLine: getLine
                       ,map: map
                       ,mapIO: mapIO
                       ,pure: pure
                       ,apply: apply
                       ,bind: bind
                       ,seq: seq
                       ,forever: forever
                       ,mapF: mapF
                       ,foldIO: foldIO
                       ,PutC: PutC
                       ,GetC: GetC
                       ,Exit: Exit
                       ,Pure: Pure
                       ,Impure: Impure};
   return _elm.IO.IO.values;
};
// Elm goes there ^
(function(){
    var stdin = process.stdin;
    var handle = function(request) {
        // Debugging:
        //console.log("Bleh: %j", request);
        if (!(request.mPut === null)) {
            process.stdout.write(request.mPut);
        } else if (!(request.mExit === null)) {
            process.exit(request.mExit);
        } else if (request.mGet) {
            stdin.resume();
        }
    }
    var handler = function(reqs) {
        for (var i = 0; i < reqs.length; i++) {
            handle(reqs[i]);
        }
    }
    var worker = Elm.worker(Elm.Main
                            , {responses: null }
                           );
    worker.ports.requests.subscribe(handler);
    
    // Read
    stdin.on('data', function(chunk) {
        //console.log('Got' + chunk);
        stdin.pause();
        worker.ports.responses.send(chunk.toString());
    })

    // Start msg
    worker.ports.responses.send(null);
})();
} // Close the callback
// Run!
jsdom.env('<p>bleh</p>', [], callback);
