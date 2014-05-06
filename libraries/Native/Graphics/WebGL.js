Elm.Native.Graphics.WebGL = {};
Elm.Native.Graphics.WebGL.make = function(elm) {

  // LOG LEVEL

  elm.Native = elm.Native || {};
  elm.Native.Graphics = elm.Native.Graphics || {};
  elm.Native.Graphics.WebGL = elm.Native.Graphics.WebGL || {};
  if (elm.Native.Graphics.WebGL.values) return elm.Native.Graphics.WebGL.values;

  var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
  var newElement = Elm.Graphics.Element.make(elm).newElement;

  var List = Elm.Native.List.make(elm);
  var MJS = Elm.Native.MJS.make(elm);
  var Utils = Elm.Native.Utils.make(elm);
  var Signal = Elm.Signal.make(elm);
  var Tuple2 = Utils.Tuple2;

  function unsafeCoerceGLSL(src) {
    return { src : src };
  }

  function loadTex(source) {

    var response = Signal.constant(elm.Http.values.Waiting);

    var img = new Image();

    img.onload = function() {
      var success = elm.Http.values.Success({img:img});
      elm.notify(response.id, success);
    }

    img.onerror = function(e) {
      var failure = A2(elm.Http.values.Failure,0,"Failed");
      elm.notify(response.id, failure);
    }

    img.src = source;

    return response;

  }

  function model(vert, frag, buffer, uniforms) {

    return model = {
      vert: vert,
      frag: frag,
      buffer: buffer,
      uniforms: uniforms
    };

  }

  function do_texture (gl, img) {

    var tex = gl.createTexture();
    console.log("Created texture");
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.generateMipmap(gl.TEXTURE_2D);
    //gl.bindTexture(gl.TEXTURE0, null);
    return tex;

  }

  function do_compile (gl, src, tipe) {

    var shader = gl.createShader(tipe);
    console.log("Created shader");

    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    var compile = gl.COMPILE_STATUS;
    if (!gl.getShaderParameter(shader,compile)) {
      throw gl.getShaderInfoLog(shader);
    }

    return shader;

  }

  function do_link (gl, vshader, fshader) {

    var program = gl.createProgram();
    console.log("Created program");

    gl.attachShader(program, vshader);
    gl.attachShader(program, fshader);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      throw gl.getProgramInfoLog(program);
    }

    return program;

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

    var numIndices = 3 * List.length(bufferElems);
    var indices = List.toArray(List.range(0, numIndices - 1));
    console.log("Created index buffer");
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
    console.log("Drawing");

    function drawModel(m) {

      var program;
      if (m.vert.id && m.frag.id) {
        var progid = m.vert.id + '#' + m.frag.id;
        program = model.cache.programs[progid];
      }

      if (!program) {

        var vshader = undefined;
        if (m.vert.id) {
          vshader = model.cache.shaders[m.vert.id];
        } else {
          m.vert.id = Utils.guid();
        }

        if (!vshader) {
          vshader = do_compile(gl, m.vert.src, gl.VERTEX_SHADER);
          model.cache.shaders[m.vert.id] = vshader;
        }

        var fshader = undefined;
        if (m.frag.id) {
          fshader = model.cache.shaders[m.frag.id];
        } else {
          m.frag.id = Utils.guid();
        }

        if (!fshader) {
          fshader = do_compile(gl, m.frag.src, gl.FRAGMENT_SHADER);
          model.cache.shaders[m.frag.id] = fshader;
        }

        program = do_link(gl, vshader, fshader);
        var progid = m.vert.id + '#' + m.frag.id;
        model.cache.programs[progid] = program;

      }

      gl.useProgram(program);

      var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
      var textureCounter = 0;
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
          case gl.SAMPLER_2D:
            var texture = m.uniforms[uniform.name];
            var tex = undefined;
            if (texture.id) {
              tex = model.cache.textures[texture.id];
            } else {
              texture.id = Utils.guid();
            }
            if (!tex) {
              tex = do_texture(gl, texture.img);
              model.cache.textures[texture.id] = tex;
            }
            var activeName = 'TEXTURE' + textureCounter;
            gl.activeTexture(gl[activeName]);
            gl.bindTexture(gl.TEXTURE_2D,tex);
            gl.uniform1i(uniformLocation, textureCounter);
            textureCounter += 1;
            break;
          default:
            console.log("Unsupported uniform type: " + uniform.type);
            break;
        }
      }

      var buffer = model.cache.buffers[m.buffer.guid];
      if (!buffer) {
        buffer = do_bind(gl, program, m.buffer);
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
        var attributeBuffer = buffer.buffers[attribute.name];

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
      model.cache.shaders = [];
      model.cache.programs = {};
      model.cache.buffers = [];
      model.cache.textures = [];

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
        h: h
      }
    };

    return A3(newElement, w, h, elem);

  }

  return elm.Native.Graphics.WebGL.values = {
    unsafeCoerceGLSL:unsafeCoerceGLSL,
    loadTex:loadTex,
    model:F4(model),
    webgl:F2(webgl)
  };

};
