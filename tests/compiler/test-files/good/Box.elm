
module Box where

import MJS (V3,M4x4,v3,makeRotate,makePerspective,makeLookAt,mul)
import Graphics.WebGL (Triangle, zipTriangle, Shader, Model, model, webgl)
import Window(dimensions)

-- Define what our geometry looks like

p0 = v3  1  1  1
p1 = v3 -1  1  1
p2 = v3 -1 -1  1
p3 = v3  1 -1  1
p4 = v3  1 -1 -1
p5 = v3  1  1 -1
p6 = v3 -1  1 -1
p7 = v3 -1 -1 -1

front  = [(p0,p1,p2),(p2,p3,p0)]
back   = [(p5,p6,p7),(p7,p4,p5)]
right  = [(p0,p3,p4),(p4,p5,p0)]
left   = [(p1,p2,p7),(p7,p6,p1)]
top    = [(p0,p5,p6),(p6,p1,p0)]
bottom = [(p3,p4,p7),(p7,p2,p3)]

positions = front ++ back ++ right ++ left ++ top ++ bottom

toV3 color =
  let c = toRgb color
  in  v3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)

colors : [Triangle V3]
colors =
  concatMap (\c' -> let c = toV3 c' in repeat 2 (c,c,c))
  [gray,red,green,blue,yellow,purple]

mesh : [Triangle {pos : V3, color : V3}]
mesh = zipWith (zipTriangle (\pos color -> { pos = pos, color = color })) positions colors

per : M4x4
per = makePerspective 45 1 0.01 100

cam : M4x4
cam = makeLookAt (v3 0 0 5) (v3 0 0 0) (v3 0 1 0)

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 10000) 0 (fps 25)

rot : Signal { rot : M4x4, per : M4x4, cam : M4x4, shade : Float }
rot = (\t -> { rot = mul (makeRotate (3*t) (v3 0 1 0)) (makeRotate (2*t) (v3 1 0 0)), per = per, cam = cam, shade = 0.8 }) <~ angle

draw : Signal [Model]
draw = combine [ model vertexShader fragmentShader mesh <~ rot ]

main = webgl <~ dimensions ~ draw

vertexShader : Shader {a | pos : V3, color : V3} {b | rot : M4x4, per : M4x4, cam : M4x4} {vcolor : V3}
vertexShader = [glShader|

attribute vec3 pos;
attribute vec3 color;
uniform mat4 per;
uniform mat4 cam;
uniform mat4 rot;
varying vec3 vcolor;
void main () {
    gl_Position = per * cam * rot * vec4(pos, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} {b | shade : Float} {vcolor : V3}
fragmentShader = [glShader|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
