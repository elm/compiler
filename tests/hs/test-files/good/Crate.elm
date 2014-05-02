
module Crate where

import Http (..)
import MJS (..)
import Graphics.WebGL (..)

pul = {pos=v3 -1  1 0, coord=v3 0 1 0}
pur = {pos=v3  1  1 0, coord=v3 1 1 0}
pbl = {pos=v3 -1 -1 0, coord=v3 0 0 0}
pbr = {pos=v3  1 -1 0, coord=v3 1 0 0}

face : [Triangle { pos : V3, coord : V3 }]
face = [(pul,pur,pbl),(pbl,pur,pbr)]

rotFace : Float -> Float -> [Triangle { pos : V3, coord : V3 }]
rotFace turnH turnV = 
  let x = makeRotate (turnV * pi / 2) (v3 1 0 0)
      y = makeRotate (turnH * pi / 2) (v3 0 1 0)
      s = makeTranslate (v3 0 0 1)
      t = mul (mul x y) s
  in map (mapTriangle (\x -> {x | pos <- mul4x4 t x.pos })) face

mesh : [Triangle { pos : V3, coord : V3 }]
mesh = concat . map (uncurry rotFace) <|
        [ (0,0)
        , (1,0)
        , (2,0)
        , (3,0)
        , (0,1)
        , (0,-1)
        ]

vert : Shader { pos : V3, coord : V3 } { u | view : M4x4 } { vcoord : V2 }
vert = [glShader|
attribute vec3 pos;
attribute vec3 coord;
uniform mat4 view;
varying vec2 vcoord;
void main () {
  gl_Position = view * vec4(pos, 1.0);
  vcoord = coord.xy;
}
|]

frag : Shader {} { u | crate : Texture } { vcoord : V2 }
frag = [glShader|
precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;
void main () {
  gl_FragColor = texture2D(crate, vcoord);
}
|]

crateTex : Signal (Response Texture)
crateTex = loadTex "woodCrate.JPG"

per : M4x4
per = makePerspective 45 1 0.01 100

cam : M4x4
cam = makeLookAt (v3 0 0 5) (v3 0 0 0) (v3 0 1 0)

angle : Signal Float
angle = foldp (\_ n -> n + 0.02) 0 (fps 25)

view : Float -> M4x4
view t = 
  let x = makeRotate (2*t) (v3 1 0 0)
      y = makeRotate (3*t) (v3 0 1 0)
  in foldr mul identity [per,cam,y,x]

content : Response Texture -> M4x4 -> [Model]
content response view = case response of
  Waiting -> []
  Failure i s -> []
  Success tex -> [model vert frag mesh { crate = tex, view = view }]

main = webgl (400,400) <~ (content <~ crateTex ~ (view <~ angle))

--main = asText (List.length mesh)


