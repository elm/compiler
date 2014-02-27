
module Crate where

import Http (..)
import MJS (..)
import Graphics.WebGL (..)

pul = v3 -1  1 0
pur = v3  1  1 0
pbl = v3 -1 -1 0
pbr = v3  1 -1 0

mesh : [Triangle { pos : V3 }]
mesh = map (mapTriangle (\pos -> {pos=pos})) <|
        [(pul,pur,pbl),(pbl,pur,pbr)]

vert : Shader { pos : V3 } u { coord : V2 }
vert = [glShader|
attribute vec3 pos;
varying vec2 coord;
void main () {
  gl_Position = vec4(pos, 1.0);
  coord = pos.xy;
}
|]

frag : Shader {} { u | crate : Texture } { coord : V2 }
frag = [glShader|
precision mediump float;
uniform sampler2D crate;
varying vec2 coord;
void main () {
  gl_FragColor = texture2D(crate, coord);
}
|]

crateTex : Signal (Response Texture)
crateTex = loadTex "woodCrate.JPG"

content : Response Texture -> [Model]
content response = case response of
  Waiting -> []
  Failure i s -> []
  Success tex -> [model vert frag mesh { crate = tex }]

main = webgl (400,400) <~ (content <~ crateTex)


