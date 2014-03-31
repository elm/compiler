Elm.Native.Array = {};
Elm.Native.Array.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Array = elm.Native.Array || {};
    if (elm.Native.Array.values) return elm.Native.Array.values;
    if ('values' in Elm.Native.Array)
      return elm.Native.Array.values = Elm.Native.Array.values;

    var List = Elm.Native.List.make(elm);

    // An RRB-Tree has two distinct data types. A leaf which contains data as
    // an array in _1, and a height in _0, that is always 0. A node has in
    // addition its size table in _2, while _1 contains an array of nodes or
    // leaves.

    // M is the maximal node size. 32 seems fast. E is the allowed increase
    // of search steps when concatting to find an index. Lower values will 
    // decrease balancing, but will increase search steps.
    var M = 32;
    var E = 2;

    // An empty array.
    var empty = { ctor:"_Array", _0:0, _1:new Array() };

    // Gets the value at index i recursively.
    function get(i, a) {
      if (a._0 == 0) {
        if (i < a._1.length) {
          return a._1[i];
        } else {
          throw new Error("Index "+ i +" on the array is out of range. Check the length first or you safeGet.");
        }
      }

      var slot = getSlot(i, a);
      var sub = slot > 0 ? a._2[slot-1] : 0;
      return get(i - sub, a._1[slot]);
    }

    // Sets the value at the index i. Only the nodes leading to i will get
    // copied and updated.
    function set(i, item, a) {
      if (length(a) <= i) {
        return a;
      }
      var newA = nodeCopy(a);
      newA._1 = a._1.slice();

      if (a._0 == 0) {
        newA._1[i] = item;
      } else {
        var slot = getSlot(i, a);
        var sub = slot > 0 ? a._2[slot-1] : 0;
        newA._1[slot] = set(i - sub, item, a._1[slot]);
      }
      return newA;
    }

    // Pushes an item via push_ to the bottom right of a tree.
    function push(item, a) {
      var pushed = push_(item, a);
      if (pushed !== null) {
        return pushed;
      }

      newTree = create(item, a._0);
      return siblise(a, newTree);
    }

    // Recursively tries to push an item to the bottom-right most
    // tree possible. If there is no space left for the item,
    // null will be returned.
    function push_(item, a) {
      // Handle resursion stop at leaf level.
      if (a._0 == 0) {
        if (a._1.length < M) {
          var newA = { ctor:"_Array", _0:0, _1:a._1.slice() };
          newA._1.push(item);
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
        newA._1[newA._1.length - 1] = pushed;
        newA._2[newA._2.length - 1]++;
        return newA
      }

      // When there was no space left, check if there is space left
      // for a new slot with a tree which contains only the item
      // at the bottom.
      if (a._1.length < M) {
        var newSlot = create(item, a._0 - 1);
        var newA = nodeCopy(a);
        newA._1.push(newSlot);
        newA._2.push(newA._2[newA._2.length - 1] + length(newSlot));
        return newA
      } else {
        return null;
      }
    }

    // Converts an array into a list of elements.
    function elements(a) {
      return elements_(List.Nil, a);
    }

    function elements_(list, a) {
      for (var i = a._1.length - 1; i >= 0; i--) {
        list = a._0 == 0 ? List.Cons(a._1[i], list) : elements_(list, a._1[i]);
      }
      return list;
    }

    // Maps a function over an array.
    function map(f, a) {
      var newA = { ctor:"_Array", _0:a._0, _1:new Array(a._1) };
      if (a._0 > 0) { newA._2 = a._2; }
      for (var i = 0; i < a._1.length; i++) {
        newA._1[i] = a._0 == 0 ? f(a._1[i]) : map(f, a._1[i]);
      }
      return newA;
    }

    function assocMap(f, a) {
      var newA = { ctor:"_Array", _0:a._0, _1:new Array(a._1) };
      if (a._0 > 0) { newA._2 = a._2; }
      for (var i = 0; i < a._1.length; i++) {
        newA._1[i] = a._0 == 0 ? A2(f, i, a._1[i]) : assocMap(f, a._1[i]);
      }
      return newA;
    }

    function foldl(f, b, a) {
      for (var i = a._1.length - 1; i >= 0; i--) {
        b = A2(f, a._0 == 0 ? a._1[i] : foldl(f, b, a._1[i]), b);
      }
      return b;
    }

    function foldr(f, b, a) {
      for (var i = 0; i < a._1.length; i++) {
        b = A2(f, a._0 == 0 ? a._1[i] : foldr(f, b, a._1[i]), b);
      }
      return b;
    }

    // Returns a sliced tree. The to is inclusive, but this may change,
    // when I understand, why e.g. JS does not handle it this way. :-)
    // If from or to is negative, they will select from the end on.
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
      if (a._0 == 0) {
        var newA = { ctor:"_Array", _0:0 };
        newA._1 = a._1.slice(0, to + 1);
        return newA;
      }

      // Slice the right recursively.
      var right = getSlot(to, a);
      var sliced = sliceRight(to - (right > 0 ? a._2[right - 1] : 0), a._1[right]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (right == 0) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", _0:a._0
                                , _1:a._1.slice(0, right + 1)
                                , _2:a._2.slice(0, right + 1) };
      newA._1[right] = sliced;
      newA._2[right] = length(sliced) + (right > 0 ? newA._2[right - 1] : 0);
      return newA;
    }

    function sliceLeft(from, a) {
      if (from == 0) {
        return a;
      }

      // Handle leaf level.
      if (a._0 == 0) {
        var newA = { ctor:"_Array", _0:0 };
        newA._1 = a._1.slice(from, a._1.length + 1);
        return newA;
      }

      // Slice the left recursively.
      var left = getSlot(from, a);
      var sliced = sliceLeft(from - (left > 0 ? a._2[left - 1] : 0), a._1[left]);

      // Maybe the a node is not even needed, as sliced contains the whole slice.
      if (left == a._1.length - 1) {
        return sliced;
      }

      // Create new node.
      var newA = { ctor:"_Array", _0:a._0
                                , _1:a._1.slice(left, a._1.length + 1)
                                , _2:new Array(a._1.length - left) };
      newA._1[left] = sliced;
      var len = 0;
      for (var i = 0; i < newA._1.length; i++) {
        len += length(newA._1[i]);
        newA._2[i] = len;
      }

      return newA;
    }

    // Concats two trees.
    // TODO: Add support for concatting trees of different sizes. Current
    // behavior will just rise the lower tree and then concat them.
    function concat(a, b) {
      if (b._0 > a._0) { return concat(parentise(a, b._0), b); }
      if (a._0 > b._0) { return concat(a, parentise(b, a._0)); }
      if (a._0 == 0) { return concat(parentise(a, 1), parentise(b, 1)); }

      var c = concat_(a, b);
      if (c[1]._1.length > 0) {
        return siblise(c[0], c[1]);
      } else {
        return c[0];
      }
    }

    // Returns an array of two nodes. The second node _may_ be empty. This case
    // needs to be handled by the function, that called concat_. May be only
    // called for trees with an minimal height of 1.
    function concat_(a, b) {
      if (a._0 == 1) {
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
      a._1[a._1.length - 1] = concated[0];
      a._2[a._2.length - 1] = length(concated[0])
      a._2[a._2.length - 1] += a._2.length > 1 ? a._2[a._2.length - 2] : 0;

      // Adjust the bottom left side of the new tree.
      if (concated[1]._1.length > 0) {
        b._1[0] = concated[1];
        b._2[0] = length(concated[1]);
        for (var i = 1, len = length(b._1[0]); i < b._2.length; i++) {
          len += length(b._1[i]);
          b._2[i] = len;
        }
      } else {
        b._1.shift();
        for (var i = 1; i < b._2.length; i++) {
          b._2[i] = b._2[i] - b._2[0];
        }
        b._2.shift();
      }

      // Check if balancing is needed and return based on that.
      var toRemove = calcToRemove(a, b);
      if (toRemove <= E || b._1.length == 0) {
        return [a,b];
      }

      return shuffle(a, b, toRemove);
    }

    // Returns the extra search steps for E. Refer to the paper.
    function calcToRemove(a, b) {
      var subLengths = 0;
      for (var i = 0; i < a._1.length; i++) {
        subLengths += a._1[i]._1.length;
      }
      for (var i = 0; i < b._1.length; i++) {
        subLengths += b._1[i]._1.length;
      }

      var toRemove = a._1.length + b._1.length
      return toRemove - (Math.floor((subLengths - 1) / M) + 1);
    }

    // get2 and set2 are helpers for accessing over two arrays.
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

    // Creates a node or leaf with a given length at their arrays for perfomance.
    // Is only used by shuffle.
    function createNode(height, length) {
      if (length < 0) { length = 0; }
      var a = { ctor:"_Array", _0:height, _1:new Array(length) };
      if (height > 0) {
        a._2 = new Array(length);
      }
      return a;
    }

    function saveSlot(a, b, index, slot) {
      set2(a._1, b._1, index, slot);

      var l = (index == 0 || index == a._2.length) ?
                0 : get2(a._2, a._2, index - 1);
      set2(a._2, b._2, index, l + length(slot));
    }

    // Returns an array of two balanced nodes.
    function shuffle(a, b, toRemove) {
      var newA = createNode(a._0, Math.min(M, a._1.length + b._1.length - toRemove));
      var newB = createNode(a._0, newA._1.length - (a._1.length + b._1.length - toRemove));

      // Skip the slots with size M. More precise: copy the slot references
      // to the new node
      var read = 0;
      while (get2(a._1, b._1, read)._1.length % M == 0) {
        set2(newA._1, newB._1, read, get2(a._1, b._1, read));
        set2(newA._2, newB._2, read, get2(a._2, b._2, read));
        read++;
      }

      // Pulling items from left to right, caching in a slot before writing
      // it into the new nodes.
      var write = read;
      var slot = new createNode(a._0 - 1, 0);
      var from = 0;

      // If the current slot is still containing data, then there will be at
      // least one more write, so we do not break this loop yet.
      while (read - write - (slot._1.length > 0 ? 1 : 0) < toRemove) {
        // Find out the max possible items for copying.
        var source = get2(a._1, b._1, read);
        var to = Math.min(M - slot._1.length, source._1.length)

        // Copy and adjust size table.
        slot._1 = slot._1.concat(source._1.slice(from, to));
        if (slot._0 > 0) {
          var len = slot._2.length;
          for (var i = len; i < len + to - from; i++) {
            slot._2[i] = length(slot._1[i]);
            slot._2[i] += (i > 0 ? slot._2[i - 1] : 0);
          }
        }

        from += to;

        // Only proceed to next slots[i] if the current one was
        // fully copied.
        if (source._1.length <= to) {
          read++; from = 0;
        }

        // Only create a new slot if the current one is filled up.
        if (slot._1.length == M) {
          saveSlot(newA, newB, write, slot);
          slot = createNode(a._0 - 1,0);
          write++;
        }
      }

      // Cleanup after the loop. Copy the last slot into the new nodes.
      if (slot._1.length > 0) {
        saveSlot(newA, newB, write, slot);
        write++;
      }

      // Shift the untouched slots to the left
      while (read < a._1.length + b._1.length ) {
        saveSlot(newA, newB, write, get2(a._1, b._1, read));
        read++; write++;
      }

      return [newA, newB];
    }

    // Helper functions
    function botRight(a) { return a._1[a._1.length - 1]; }
    function botLeft(a)  { return a._1[0]; }

    // Copies a node for updating. Note that you should not use
    // this if only updating one of _1 and _2 for performance reasons.
    function nodeCopy(a) {
      var newA = { ctor:"_Array", _0:a._0
                                , _1:a._1.slice() };
      if (a._0 > 0) { newA._2 = a._2.slice(); }
      return newA;
    }

    // Returns how many items are in the tree.
    function length(a) {
      if (a._0 == 0) {
        return a._1.length;
      } else {
        return a._2[a._2.length - 1];
      }
    }

    // Calculates in which slot the item probably is, then
    // find the exact slot in the size table ._2. Returns the index.
    function getSlot(i, a) {
      var slot = Math.floor(i / (Math.pow(M, a._0)));
      while (a._2[slot] <= i) {
        slot++
      }
      return slot;
    }

    // Recursively creates a tree with a given height containing
    // only the given item.
    function create(item, height) {
      if (height == 0) {
        return { ctor:"_Array", _0:0
                              , _1:[item] };
      } else {
        return { ctor:"_Array", _0:height
                              , _1:[create(item, height - 1)]
                              , _2:[1] };
      }
    }

    // Recursively creates a tree that contains the given tree.
    function parentise(tree, height) {
      if (height == tree._0) {
        return tree;
      } else {
        return { ctor:"_Array", _0:height
                              , _1:[parentise(tree, height - 1)]
                              , _2:[length(tree)] };
      }
    }

    // Emphasizes blood brotherhood beneath two trees.
    function siblise(a, b) {
      return { ctor:"_Array", _0:a._0 + 1
                            , _1:[a, b]
                            , _2:[length(a), length(a) + length(b)] };
    }

    Elm.Native.Array.values = {
      empty:empty,
      elements:elements,
      concat:F2(concat),
      push:F2(push),
      slice:F3(slice),
      get:F2(get),
      set:F3(set),
      map:F2(map),
      assocMap:F2(assocMap),
      foldl:F3(foldl),
      foldr:F3(foldr),
      length:length
    };

    return elm.Native.Array.values = Elm.Native.Array.values;
}
