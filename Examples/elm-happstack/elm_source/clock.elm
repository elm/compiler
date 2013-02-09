
import Time (every)

hand clr len time =
  let t = pi * time / 30 - pi / 2 in
  solid clr $ line [(200,200), (200 + len * cos t, 200 + len * sin t)]

clock t = collage 400 400 [ filled (rgb 96 176 224) $ ngon 12 110 (200, 200)
                          , hand  red  100 t
                          , hand black 100 (t/60)
                          , hand black 60  (t/720) ]

main = lift clock (every 1)
