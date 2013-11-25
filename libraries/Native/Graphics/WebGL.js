Elm.Native.Graphics.WebGL = {};
Elm.Native.Graphics.WebGL.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Graphics = elm.Native.Graphics || {};
    elm.Native.Graphics.WebGL = elm.Native.Graphics.WebGL || {};
    if (elm.Native.Graphics.WebGL.values) return elm.Native.Graphics.WebGL.values;

    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

    var Signal = Elm.Signal.make(elm);
    var newElement = Elm.Graphics.Element.make(elm).newElement;
    var Utils = Elm.Native.Utils.make(elm);
    var Tuple2 = Utils.Tuple2;
    var MJS = Elm.Native.MJS.make(elm);
    var List = Elm.Native.List.make(elm);

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
                   uniform mat4 uSceneMatrix;\
                   \
                   varying vec4 vVertexColor;\
                   \
                   void main() {\
                       gl_Position = uSceneMatrix * vec4(aVertexPosition, 1.0);\
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

        var gl = model.gl;
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        var program = model.program;

        function drawMesh(mesh, sceneMatrix) {
            gl.useProgram(program.programPtr);
            
            gl.enableVertexAttribArray(program.vertexPositionAttribute);
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, mesh.indexBuffer);
            gl.bindBuffer(gl.ARRAY_BUFFER, mesh.vertexPosBuffer);
            gl.vertexAttribPointer(program.vertexPositionAttribute, 3, gl.FLOAT, false, 0, 0);

            gl.enableVertexAttribArray(program.vertexColorAttribute);
            gl.bindBuffer(gl.ARRAY_BUFFER, mesh.vertexColorBuffer);
            gl.vertexAttribPointer(program.vertexColorAttribute, 4, gl.FLOAT, false, 0, 0);

            gl.uniformMatrix4fv(program.sceneMatrixUniform, false, sceneMatrix);
            
            gl.drawElements(gl.TRIANGLES, mesh.numIndices, gl.UNSIGNED_SHORT, 0);
        }

        var sceneMatrixStack = [MJS.M4x4.identity];

        function drawScene(scene,n) {
            switch(scene.ctor) {
                case 'SceneLeaf':
                    drawMesh(scene._0, sceneMatrixStack[n]);
                    break;
                default:
                    var m = scene._0;
                    sceneMatrixStack[n+1] = MJS.M4x4.mul(sceneMatrixStack[n],m)
                    function drawChild(c) {
                        drawScene(c,n+1);
                    }
                    var children = scene._1;
                    List.each(drawChild, children);
                    break;
            }
        }
        
        var scene = model.scene;
        drawScene(scene,0);

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

    function makeGL(w,h) {
        var node = newNode('canvas');
        var gl = node.getContext('webgl') || node.getContext('experimental-webgl');
        var programPtr = createProgram(gl, geoVert, geoFrag);
        var vertexPositionAttribute = gl.getAttribLocation(programPtr,'aVertexPosition');
        var vertexColorAttribute = gl.getAttribLocation(programPtr,'aVertexColor');
        var sceneMatrixUniform = gl.getUniformLocation(programPtr, 'uSceneMatrix');
        var program = {
            programPtr: programPtr,
            vertexPositionAttribute: vertexPositionAttribute,
            vertexColorAttribute: vertexColorAttribute,
            sceneMatrixUniform: sceneMatrixUniform,
        };
        return Signal.constant({w:w,h:h,gl:gl,node:node,program:program});
    }

    function bufferMesh(triangles, context) {
        var indices = [];
        var positions = [];
        var colors = [];

        function pushTriangle(n, node) {
            if (node.ctor === "[]") return;
            var tri = node._0;
            for (var i = 0; i < 3; i+= 1) {
                indices.push(3 * n + i);
                var vertex = tri['_' + i];
                for (var j = 0; j < 3; j += 1) {
                    positions.push(vertex.pos[j]);
                }
                colors.push(vertex.color._0 / 256);
                colors.push(vertex.color._1 / 256);
                colors.push(vertex.color._2 / 256);
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

        var vertexColorBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, vertexColorBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(colors), gl.STATIC_DRAW);

        return {
            indexBuffer: indexBuffer,
            vertexPosBuffer: vertexPosBuffer,
            vertexColorBuffer: vertexColorBuffer,
            numIndices: indices.length,
        };

    }

    function webgl(context, scene) {
        
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
        makeGL:F2(makeGL),
        bufferMesh:F2(bufferMesh),
        webgl:F2(webgl),
    };

};
