
import MJS (V3,v3,m4x4makeRotate)
import Graphics.WebGL (GL, Model, link, bind, encapsulate, webgl)

-- Define what our geometry looks like

top = v3 0 1 0
left = v3 -1 -1 0
right = v3 1 -1 0

mesh : [{ pos : V3 }]
mesh = map (\vec -> { pos = vec }) [top, left, right]

vert = [glShader|
attribute vec3 pos;
uniform mat4 rot;
void main () {
    gl_Position = rot * vec4(pos, 1.0);
}
|]

frag = [glShader|
void main () {
    gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
|]

draw : Signal Float -> Signal GL -> Signal [Model]
draw theta gl = 
    let prog = link <~ gl ~ constant vert ~ constant frag
        buf = bind <~ gl ~ prog ~ constant mesh
        rot = (\t vec -> { rot = m4x4makeRotate t vec }) <~ theta ~ constant (v3 0 1 0)
    in combine [ encapsulate <~ prog ~ buf ~ rot ]

angle = foldp (\_ n -> n + 0.1) 0 (fps 25)

main = webgl (constant (400,400)) (draw angle)

