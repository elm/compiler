
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

gray   = v3 0.7 0.7 0.7
red     = v3 1 0 0
green   = v3 0 1 0
blue    = v3 0 0 1
yellow  = v3 1 1 0
purple  = v3 0 1 1

repeat n elem = map (\_ -> elem) [0..n-1]

colors = concat . map (\c -> repeat 2 (c,c,c)) <| [gray,red,green,blue,yellow,purple]

mesh : [Triangle {pos : V3, color : V3}]
mesh = zipWith (zipTriangle (\pos color -> { pos = pos, color = color })) positions colors

vert : Shader {a | pos : V3, color : V3} {b | rot : M4x4, per : M4x4, cam : M4x4} {vcolor : V3}
vert = [glShader|
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

frag : Shader {} {b | shade : Float} {vcolor : V3}
frag = [glShader|
precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}
|]

per : M4x4
per = makePerspective 45 1 0.01 100

cam : M4x4
cam = makeLookAt (v3 0 0 5) (v3 0 0 0) (v3 0 1 0)

angle : Signal Float
angle = foldp (\_ n -> n + 0.02) 0 (fps 25)

rot : Signal { rot : M4x4, per : M4x4, cam : M4x4, shade : Float }
rot = (\t -> { rot = mul (makeRotate (3*t) (v3 0 1 0)) (makeRotate (2*t) (v3 1 0 0)), per = per, cam = cam, shade = 0.8 }) <~ angle

draw : Signal [Model]
draw = combine [ (\rot -> (model vert frag mesh rot)) <~ rot ]

main = webgl <~ dimensions ~ draw

