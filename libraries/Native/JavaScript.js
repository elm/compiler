
Elm.Native.JavaScript = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.JavaScript) return elm.Native.JavaScript;

  var List = Elm.Native.List(elm);
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);

  function fromJS(v) {
      var type = typeof v;
      if (type === 'number' ) return v;
      if (type === 'boolean') return v;
      if (type === 'string' ) return List.fromArray(v);
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
      if (type === 'number' || type === 'boolean') return v;
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
          if (typeof array[0] === 'string') {
              array = array.join('');
          } else {
              for (var i = array.length; i--; ) {
                  array[i] = toJS(array[i]);
              }
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

  function id(n) { return n; }

  function toElement(w,h,domNode) {
      return A3( newElement, w, h, {
              ctor: 'Custom',
              type: 'DomNode',
              render: function(node) { return node; },
              update: function(node,oldNode,newNode) {
                  if (node === newNode) return;
                  node.parentNode.replaceChild(newNode, node);
              },
              model: domNode
          });
  }

  function fromElement(element) {
      return Render.render(element);
  }

  return elm.Native.JavaScript = {
      toFloat    : id,
      toBool     : id,
      toInt      : function(n) { return n|0; },
      toString   : List.fromArray,
      toList     : List.fromArray,
      fromString : function(s) { return List.toArray(s).join(''); },
      fromList   : List.toArray,
      fromInt    : id,
      fromFloat  : id,
      fromBool   : id,

      toElement   : toElement,
      fromElement : fromElement,
      toRecord    : fromJS,
      fromRecord  : fromRecord
  };

};
