import Graphics.Input
(f,fs) = Graphics.Input.fieldMultiline "Foo"
main = (\f fs->flow down [f,plainText fs]) <~ f ~ fs