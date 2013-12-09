
import MJS (V3,v3)
import Graphics.WebGL (link, bind, encapsulate, webgl)

-- Define what our geometry looks like

top : V3
top = v3 0 1 0
left : V3
left = v3 -1 -1 0
right : V3
right = v3 1 -1 0

mesh : [{ pos : V3 }]
mesh = map (\vec -> { pos = vec }) [top, left, right]

vert = [glShader|
attribute vec3 pos;
void main () {
    gl_Position = vec4(pos, 1.0);
}
|]

frag = [glShader|
void main () {
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
|]

draw gl = 
    let prog = link gl vert frag
        buf = bind gl prog mesh
    in [ encapsulate prog buf {} ]

main = webgl 400 400 draw

