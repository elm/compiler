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

    function encapsulate(program, buffer, uniforms) {

        var model = {
            program: program,
            buffer: buffer,
            uniforms: uniforms
        };

        return model;

    }

    function webgl(w, h, draw) {

        var node = newNode('canvas');
        var gl = node.getContext('webgl');

        function link (vSrc, fSrc) {

            function createShader(str, type) {

                var shader = gl.createShader(type);

                gl.shaderSource(shader, str);
                gl.compileShader(shader);
                var compile = gl.COMPILE_STATUS;
                if (!gl.getShaderParameter(shader,compile)) {
                    throw gl.getShaderInfoLog(shader);
                }

                return shader;

            };

            var vshader = createShader(vSrc, gl.VERTEX_SHADER);
            var fshader = createShader(fSrc, gl.FRAGMENT_SHADER);
            var program = gl.createProgram();

            gl.attachShader(program, vshader);
            gl.attachShader(program, fshader);
            gl.linkProgram(program);
            if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
                throw gl.getProgramInfoLog(program);
            }

            return program;

        }

        function bind (program, bufferElems) {

            var bufferObject = {};

            var attributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i = 0; i < attributes; i += 1) {
                var attribute = gl.getActiveAttrib(program, i);
                switch (attribute.type) {
                    case gl.FLOAT_VEC3:

                        // Might want to invert the loop
                        // to build the array buffer first
                        // and then bind each one-at-a-time
                        var data = [];
                        List.each(function(elem){
                            data.push(elem[attribute.name][0]);
                            data.push(elem[attribute.name][1]);
                            data.push(elem[attribute.name][2]);
                        }, bufferElems);
                        var array = new Float32Array(data);

                        var buffer = gl.createBuffer();
                        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
                        gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);

                        bufferObject[attribute.name] = buffer;

                        break;

                    default:
                        console.log("Bad buffer type");
                        break;
                }

            }

            return bufferObject;

        }

        function render(model) {
            drawGL(model);
            return model.node;
        }

        function update(canvasNode, oldModel, newModel) {
            drawGL(newModel)
        }

    }

    function drawGL(model) {

        var gl = model.gl;
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        function drawModel(glModel) {
            var program = glModel.program;
            gl.useProgram(program);

            var activeUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
            for (var i = 0; i < activeUniforms; i += 1) {
                var uniform = gl.getActiveUniform(program, i);
                var uniformLocation = gl.getUniformLocation(program, uniform.name);
                switch (uniform.type) {
                    case gl.FLOAT_MAT4:
                        gl.uniformMatrix4fv(uniformLocation, false, glModel.uniforms[uniform.name]);
                        break;
                    default:
                        console.log("Unsupported uniform type: " + uniform.type);
                        break;
                }
            }

            var numIndices = List.length(glModel.attributes);
            var indices = List.toArray(List.range(0,numIndices));
            var indexBuffer = gl.createBuffer();
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
            gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);

            var activeAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i = 0; i < activeAttributes; i += 1) {
                var attribute = gl.getActiveAttrib(program, i);
                var attribLocation = gl.getAttribLocation(program, attribute.name);
                gl.enableVertexAttribArray(attribLocation);
                switch (attribute.type) {
                    case gl.FLOAT_VEC3:
                        var data = [];
                        List.each(function(elem){
                            data.push(elem[attribute.name][0]);
                            data.push(elem[attribute.name][1]);
                            data.push(elem[attribute.name][2]);
                        }, glModel.attributes);
                        var attributeBuffer = gl.createBuffer();
                        gl.bindBuffer(gl.ARRAY_BUFFER, attributeBuffer);
                        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data), gl.STATIC_DRAW);
                        gl.vertexAttribPointer(attribLocation, 3, gl.FLOAT, false, 0, 0);
                        break;
                    default:
                        console.log("Unsupported attribute type: " + attribute.type);
                        break;
                }
            }

            gl.drawElements(gl.TRIANGLES, numIndices, gl.UNSIGNED_SHORT, 0);

        }

        List.each(drawModel, model.glModels);

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

    function webgl(context, glModels) {

        return A3(newElement, context.w, context.h, {
            ctor: 'Custom',
               type: 'WebGL',
               render: render,
               update: update,
               model: {
                   aspect: context.w/context.h,
               node: context.node,
               gl: context.gl,
               glModels: glModels,
               },
        });

    }

    return elm.Native.Graphics.WebGL.values = {
        encapsulate:F3(linkModel),
        webgl:F3(webgl),
    };

};
