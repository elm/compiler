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

    function link (gl, vSrc, fSrc) {

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

    function bind (gl, program, bufferElems) {

        var buffers = {};

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
                    console.log("Created attribute buffer " + attribute.name);
                    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
                    gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);

                    buffers[attribute.name] = buffer;

                    break;

                default:
                    console.log("Bad buffer type");
                    break;
            }

        }

        var bufferObject = {
            numIndices: List.length(bufferElems),
            buffers: buffers
        };

        return bufferObject;

    }

    function drawGL(gl,state) {

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        function drawModel(model) {

            var program = model.program;
            gl.useProgram(program);

            var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
            for (var i = 0; i < numUniforms; i += 1) {
                var uniform = gl.getActiveUniform(program, i);
                var uniformLocation = gl.getUniformLocation(program, uniform.name);
                switch (uniform.type) {
                    case gl.FLOAT_MAT4:
                        gl.uniformMatrix4fv(uniformLocation, false, model.uniforms[uniform.name]);
                        break;
                    default:
                        console.log("Unsupported uniform type: " + uniform.type);
                        break;
                }
            }

            var numIndices = model.buffer.numIndices;
            var indices = List.toArray(List.range(0,numIndices));

            console.log("Created index buffer");
            var indexBuffer = gl.createBuffer();
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
            gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);

            var numAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i = 0; i < numAttributes; i += 1) {
                var attribute = gl.getActiveAttrib(program, i);
                var attribLocation = gl.getAttribLocation(program, attribute.name);
                gl.enableVertexAttribArray(attribLocation);
                attributeBuffer = model.buffer.buffers[attribute.name];

                switch (attribute.type) {
                    case gl.FLOAT_VEC3:
                        gl.bindBuffer(gl.ARRAY_BUFFER, attributeBuffer);
                        gl.vertexAttribPointer(attribLocation, 3, gl.FLOAT, false, 0, 0);
                        break;
                    default:
                        console.log("Unsupported attribute type: " + attribute.type);
                        break;
                }
            }

            gl.drawElements(gl.TRIANGLES, numIndices, gl.UNSIGNED_SHORT, 0);

        }

        List.each(drawModel, state.models);

    }

    function webgl(sDimensions, sDraw) {

        var node = newNode('canvas');
        var gl = node.getContext('webgl');
        var sGL = Signal.constant(gl);

        var application = F2(function(f,x) { return f(x); });
        var sModels = A3(Signal.lift2,application,sDraw,sGL);

        function makeGLelement(dimensions, models, gl) {

            var w = dimensions._0;
            var h = dimensions._1;

            function render(state) {
                drawGL(gl,state);
                return node;
            }

            function update(_node, oldState, newState) {
                drawGL(gl,newState)
            }

            var elem = {
                ctor: 'Custom',
                type: 'WebGL',
                render: render,
                update: update,
                model: {
                    models: models,
                }
            };

            return A3(newElement, w, h, elem);

        }

        return A4(Signal.lift3,F3(makeGLelement),sDimensions,sModels,sGL);

    }

    return elm.Native.Graphics.WebGL.values = {
        link:F3(link),
        bind:F3(bind),
        encapsulate:F3(encapsulate),
        webgl:F2(webgl)
    };

};
