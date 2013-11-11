Elm.Native.Graphics.WebGL = {};
Elm.Native.Graphics.WebGL.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.WebGL = elm.Native.Graphics.WebGL || {};
    if (elm.Native.Graphics.WebGL.values) return elm.Native.Graphics.WebGL.values;

    //var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    //var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    //var JS = Elm.Native.JavaScript.make(elm);
    //var Utils = Elm.Native.Utils.make(elm);
    //var Tuple2 = Utils.Tuple2;

    function glContext(w,h,scene) {

        function drawGL(ctx,model) {
            throw "Not implemented" 
        }

        function render(model) {
            var canvas = newNode('canvas');
            gl = canvas.getContext("webgl");
            drawGL(gl,model);
            return canvas;
        }

        function update(canvasNode, _oldModel, newModel) {
            gl = canvasNode.getContext("webgl");
            drawGL(gl,newModel)
        }

        return function (scene) {
            return A3(newElement, 100, 40, {
                ctor: 'Custom',
                   type: 'WebGL',
                   render: render,
                   update: update,
                   model: scene,
            });
        }

    }

    return elm.Native.Graphics.Input.values = {
        glContext:glContext,
    };

};
