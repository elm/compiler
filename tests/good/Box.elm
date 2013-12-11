
import MJS (V3,v3,M4x4,m4x4makeRotate)
import Graphics.WebGL (Program, link, Triangle, zipTriangle, Buffer, bind, Model, encapsulate, webgl)

-- Define what our geometry looks like

topleft = v3 -1 1 0
topright = v3 1 1 0
botleft = v3 -1 -1 0
botright = v3 1 -1 0

red = v3 1 0 0
green = v3 0 1 0
blue = v3 0 0 1

colors = [(red,green,blue),(red,green,blue)]

mesh : [Triangle {pos : V3, color : V3}]
mesh = zipWith (zipTriangle (\pos color -> { pos = pos, color = color })) [(topleft, topright, botleft), (topright, botleft, botright)] colors

vert = [glShader|
attribute vec3 pos;
attribute vec3 color;
uniform mat4 rot;
varying vec3 vcolor;
void main () {
    gl_Position = rot * vec4(pos, 1.0);
    vcolor = color;
}
|]

frag = [glShader|
precision mediump float;
varying vec3 vcolor;
void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}
|]

prog : Program {pos : V3, color : V3} { rot : M4x4 }
prog = link vert frag

buf : Buffer {pos : V3, color : V3}
buf = bind mesh

angle : Signal Float
angle = foldp (\_ n -> n + 0.1) 0 (fps 25)

rot : Signal { rot : M4x4 }
rot = (\t -> { rot = m4x4makeRotate t (v3 0 1 0) }) <~ angle

draw : Signal [Model]
draw = combine [ (encapsulate prog buf) <~ rot ]

main = webgl (400,400) <~ draw

