Elm.Native.Graphics.WebGL = {};
Elm.Native.Graphics.WebGL.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.WebGL = elm.Native.Graphics.WebGL || {};
    if (elm.Native.Graphics.WebGL.values) return elm.Native.Graphics.WebGL.values;

    //var Render = ElmRuntime.use(ElmRuntime.Render.Element);
    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    //var JS = Elm.Native.JavaScript.make(elm);
    var Utils = Elm.Native.Utils.make(elm);
    var Tuple2 = Utils.Tuple2;
    var MJS = Elm.Native.MJS.make(elm);

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
        var aspect = model.aspect;

        var projectionMatrix = MJS.M4x4.makePerspective(45, aspect, 0.01, 100);

        var eye = MJS.V3.$(0,0,1);
        var center = MJS.V3.$(0,0,0);
        var up = MJS.V3.$(0,1,0);

        var viewMatrix = MJS.M4x4.makeLookAt(eye,center,up);
        
        var modelMatrixStack = [MJS.M4x4.identity];

        var gl = model.gl;
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        var scene = model.scene;
        console.log(scene);
        
    }

    /*
     * Called for new node, caches lots of stuff
     */
    function render(model) {
        drawGL(model);
        return model.node;
    }

    /*
     * Called at existing node
     */
    function update(canvasNode, oldModel, newModel) {
        drawGL(newModel)
    }

    function glContext(w,h) {
        var node = newNode('canvas');
        var gl = node.getContext('webgl');
        var program = createProgram(gl, geoVert, geoFrag);
        return Signal.constant({w:w,h:h,gl:gl,node:node,program:program});
    }

    function makeMesh(triangles, context) {
        var indices = [];
        var positions = [];
        var colors = [];

        function pushTriangle(n, node) {
            if (node.ctor === "[]") return;
            var tri = node._0;
            var names = ['a','b','c'];
            for (var i = 0; i < 3; i+= 1) {
                indices.push(3 * n + i);
                var vertex = tri[names[i]];
                for (var j = 0; j < 3; j += 1) {
                    positions.push(vertex.pos[j]);
                }
                colors.push(vertex.color._0);
                colors.push(vertex.color._1);
                colors.push(vertex.color._2);
                colors.push(vertex.color._3);
            }
            pushTriangle(n+1,node._1);
        }

        pushTriangle(0,triangles);

        var gl = context.gl;

        var indexBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);

        var vertexPosBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, vertexPosBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

        var vertexColors = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, vertexColors);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(colors), gl.STATIC_DRAW);

        return {
            indexBuffer:indexBuffer,
            vertexPosBuffer: vertexPosBuffer,
            vertexColors: vertexColors
        };

    }

    function renderGL(context, scene) {
        
        return A3(newElement, context.w, context.h, {
            ctor: 'Custom',
            type: 'WebGL',
            render: render,
            update: update,
            model: {
                aspect: context.w/context.h,
                node: context.node,
                gl: context.gl,
                program: context.program,
                scene: scene,
            },
        });

    }

    return elm.Native.Graphics.WebGL.values = {
        glContext:F2(glContext),
        makeMesh:F2(makeMesh),
        renderGL:F2(renderGL),
    };

};
