Elm.Native.Graphics.WebGL = {};
Elm.Native.Graphics.WebGL.make = function(elm) {

    // LOG LEVEL

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

        return model = {
            program: program,
            buffer: buffer,
            uniforms: uniforms
        };

    }

    function link (vSrc, fSrc) {

        var guid = Utils.guid();

        //console.log("Wrapped program #" + guid);

        return program = {
            guid: guid,
            vSrc: vSrc,
            fSrc: fSrc
        };

    }

    function do_link (gl, vSrc, fSrc) {

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
        //console.log("Created shaders and program");

        gl.attachShader(program, vshader);
        gl.attachShader(program, fshader);
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            throw gl.getProgramInfoLog(program);
        }

        return program;

    }

    function bind (bufferElems) {

        var guid = Utils.guid();

        //console.log("Wrapped buffer #" + guid);

        return buffer = {
            guid: guid,
            bufferElems: bufferElems
        };

    }

    function do_bind (gl, program, bufferElems) {

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
                        data.push(elem._0[attribute.name][0]);
                        data.push(elem._0[attribute.name][1]);
                        data.push(elem._0[attribute.name][2]);
                        data.push(elem._1[attribute.name][0]);
                        data.push(elem._1[attribute.name][1]);
                        data.push(elem._1[attribute.name][2]);
                        data.push(elem._2[attribute.name][0]);
                        data.push(elem._2[attribute.name][1]);
                        data.push(elem._2[attribute.name][2]);
                    }, bufferElems);
                    var array = new Float32Array(data);

                    var buffer = gl.createBuffer();
                    //console.log("Created attribute buffer " + attribute.name);
                    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
                    gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);

                    buffers[attribute.name] = buffer;

                    break;

                default:
                    console.log("Bad buffer type");
                    break;
            }

        }

        var numIndices = 3 * List.length(bufferElems);
        var indices = List.toArray(List.range(0, numIndices - 1));
        //console.log("Created index buffer");
        var indexBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);

        var bufferObject = {
            numIndices: numIndices,
            indexBuffer: indexBuffer,
            buffers: buffers
        };

        return bufferObject;

    }

    function drawGL(model) {
        
        var gl = model.cache.gl;

        gl.viewport(0, 0, model.w, model.h);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        //console.log("Drawing");

        function drawModel(m) {

            var program = model.cache.programs[m.program.guid];
            if (!program) {
                program = do_link(gl, m.program.vSrc, m.program.fSrc);
                model.cache.programs[m.program.guid] = program;
            }

            gl.useProgram(program);

            var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
            for (var i = 0; i < numUniforms; i += 1) {
                var uniform = gl.getActiveUniform(program, i);
                var uniformLocation = gl.getUniformLocation(program, uniform.name);
                switch (uniform.type) {
                    case gl.INT:
                        gl.uniform1i(uniformLocation, m.uniforms[uniform.name]);
                        break;
                    case gl.FLOAT:
                        gl.uniform1f(uniformLocation, m.uniforms[uniform.name]);
                        break;
                    case gl.FLOAT_VEC3:
                        gl.uniform3fv(uniformLocation, m.uniforms[uniform.name]);
                        break;
                    case gl.FLOAT_MAT4:
                        gl.uniformMatrix4fv(uniformLocation, false, m.uniforms[uniform.name]);
                        break;
                    default:
                        console.log("Unsupported uniform type: " + uniform.type);
                        break;
                }
            }

            var buffer = model.cache.buffers[m.buffer.guid];
            if (!buffer) {
                buffer = do_bind(gl, program, m.buffer.bufferElems);
                model.cache.buffers[m.buffer.guid] = buffer;
            }

            var numIndices = buffer.numIndices;
            var indexBuffer = buffer.indexBuffer;
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

            var numAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i = 0; i < numAttributes; i += 1) {
                var attribute = gl.getActiveAttrib(program, i);
                var attribLocation = gl.getAttribLocation(program, attribute.name);
                gl.enableVertexAttribArray(attribLocation);
                attributeBuffer = buffer.buffers[attribute.name];

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

        List.each(drawModel, model.models);

    }

    function webgl(dimensions, models) {

        if (!window.WebGLRenderingContext) {
            throw new Error("It appears your browser does not support WebGL! http://get.webgl.org/troubleshooting");
            return;
        }

        var w = dimensions._0;
        var h = dimensions._1;

        function render(model) {

            var div = newNode('div');
            div.style.overflow = 'hidden';
            var canvas = newNode('canvas');
            var gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
            gl.enable(gl.DEPTH_TEST);

            model.cache.gl = gl;
            model.cache.canvas = canvas;
            model.cache.programs = [];
            model.cache.buffers = [];

            update(div, model, model);

            return div;

        }

        function update(div, oldModel, newModel) {

            newModel.cache = oldModel.cache;

            var canvas = newModel.cache.canvas;

            canvas.style.width = oldModel.w + 'px';
            canvas.style.height = oldModel.h + 'px';
            canvas.style.display = "block";
            canvas.style.position = "absolute";
            canvas.width = oldModel.w;
            canvas.height = oldModel.h;

            drawGL(newModel);

            div.appendChild(canvas);

        }

        var elem = {
            ctor: 'Custom',
            type: 'WebGL',
            render: render,
            update: update,
            model: {
                models: models,
                cache: {},
                w: w,
                h: h,
            }
        };

        return A3(newElement, w, h, elem);

    }

    return elm.Native.Graphics.WebGL.values = {
        link:F2(link),
        bind:bind,
        encapsulate:F3(encapsulate),
        webgl:F2(webgl)
    };

};
