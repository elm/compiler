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

    function createShader(gl, str, type) {
        var shader = gl.createShader(type);
        gl.shaderSource(shader, str);
        gl.compileShader(shader);

        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            throw gl.getShaderInfoLog(shader);
        }

        return shader;
    };

    function linkProgram(gl, program) {
        var vshader = createShader(gl, program.vshaderSource, gl.VERTEX_SHADER);
        var fshader = createShader(gl, program.fshaderSource, gl.FRAGMENT_SHADER);
        gl.attachShader(program, vshader);
        gl.attachShader(program, fshader);
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            throw gl.getProgramInfoLog(program);
        }
    }

    function createProgram(gl, vstr, fstr) {
        var program = gl.createProgram();
        var vshader = createShader(gl, vstr, gl.VERTEX_SHADER);
        var fshader = createShader(gl, fstr, gl.FRAGMENT_SHADER);
        gl.attachShader(program, vshader);
        gl.attachShader(program, fshader);
        gl.linkProgram(program);

        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            throw gl.getProgramInfoLog(program);
        }

        return program;
    };

    var geoVert = "attribute vec3 aVertexPosition;\
                   attribute vec4 aVertexColor;\
                   \
                   uniform mat4 uMMatrix;\
                   uniform mat4 uPMatrix;\
                   uniform mat4 uVMatrix;\
                   \
                   varying vec4 vVertexColor;\
                   \
                   void main() {\
                       gl_Position = uPMatrix * uVMatrix * uMMatrix * vec4(aVertexPosition, 1.0);\
                           vVertexColor = aVertexColor;\
                   }"

    var geoFrag = "precision mediump float;\
                   \
                   varying vec4 vVertexColor;\
                   \
                   void main() {\
                       gl_FragColor = vVertexColor;\
                   }"

    function drawGL(model) {
        var scene = model.scene;
        console.log(scene);
    }

    /*
     * Called for new node, caches lots of stuff
     */
    function render(model) {
        var canvas = newNode('canvas');
        var gl = canvas.getContext("webgl");
        var program = createProgram(gl, geoVert, geoFrag);
        model.cache = {
            gl: gl,
            program: program,
        };
        drawGL(model);
        return canvas;
    }

    /*
     * Called at existing node
     */
    function update(canvasNode, oldModel, newModel) {
        newModel.cache = oldModel.cache;
        drawGL(newModel)
    }

    function glContext(w,h,scene) {

        return A3(newElement, w, h, {
            ctor: 'Custom',
               type: 'WebGL',
               render: render,
               update: update,
               model: {scene:scene},
        });

    }

    return elm.Native.Graphics.WebGL.values = {
        glContext:F3(glContext),
    };

};
