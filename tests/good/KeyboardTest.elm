import Keyboard
import Keyboard.Keys
main = (\shift ctrl space enter wasd arrows meta ->
          flow right <| map plainText
                            ["Shift:",show shift
                            ,"Ctrl:",show ctrl
                            ,"Space:",show space
                            ,"Enter:",show enter
                            ,"Arrows:",show arrows
                            ,"WASD:",show wasd
                            ,"Meta:",show meta])
                            <~ Keyboard.shift
                            ~ Keyboard.ctrl
                            ~ Keyboard.space
                            ~ Keyboard.enter
                            ~ Keyboard.wasd
                            ~ Keyboard.arrows
                            ~ (Keyboard.isKeyDown Keyboard.Keys.meta)
