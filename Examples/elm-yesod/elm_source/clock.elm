clockColor = rgb (6/16) (11/16) (14/16)

hand clr len time =
  let t = Math.PI * time / 30 - Math.PI / 2 in
  solid clr $ line [(200,200), (200 + len * cos t, 200 + len * sin t)]

clock t = collage 400 400 [ filled clockColor $ ngon 12 110 (200, 200)
                          , hand  red  100 t
                          , hand black 100 (t/60)
                          , hand black 60  (t/720) ]

main = lift clock $ Time.every 1