module Keyboard.Keys where

{-| Type alias to make it clearer what integers are supposed to represent
in this library.

You can use [`Char.toCode`](docs/Char.elm#toCode) and
[`Char.fromCode`](/docs/Char.elm#fromCode) to convert key codes to characters.
Use the uppercase character with `toCode`.
-}
type KeyCode = Int

type Key =
 {keyCode: KeyCode
 ,name: String}

a =
 {keyCode = 65
 ,name = "a"}

b =
 {keyCode = 66
 ,name = "b"}

c =
 {keyCode = 67
 ,name = "b"}

d =
 {keyCode = 68
 ,name = "d"}

e = 
 {keyCode = 69
 ,name = "e"}

f = 
 {keyCode = 70
 ,name = "f"}

g = 
 {keyCode = 71
 ,name = "g"}

h = 
 {keyCode = 72
 ,name = "h"}

i = 
 {keyCode = 73
 ,name = "i"}

j = 
 {keyCode = 74
 ,name = "j"}

k = 
 {keyCode = 75
 ,name = "k"}

l = 
 {keyCode = 76
 ,name = "l"}

m = 
 {keyCode = 77
 ,name = "m"}

n = 
 {keyCode = 78
 ,name = "n"}

o = 
 {keyCode = 79
 ,name = "o"}

p = 
 {keyCode = 80
 ,name = "p"}

q = 
 {keyCode = 81
 ,name = "q"}

r = 
 {keyCode = 82
 ,name = "r"}

s = 
 {keyCode = 83
 ,name = "s"}

t = 
 {keyCode = 84
 ,name = "t"}

u = 
 {keyCode = 85
 ,name = "u"}

v = 
 {keyCode = 86
 ,name = "v"}

w = 
 {keyCode = 87
 ,name = "w"}

x = 
 {keyCode = 88
 ,name = "x"}

y = 
 {keyCode = 89
 ,name = "y"}

z = 
 {keyCode = 90
 ,name = "z"}

ctrl = 
 {keyCode = 17
 ,name = "Ctrl"}

shift = 
 {keyCode = 16
 ,name = "Shift"}

tab = 
 {keyCode = 9
 ,name = "Tab"}

super = 
 {keyCode = 91
 ,name = "Super"}

meta = 
 {keyCode = 91
 ,name = "Meta"}

windows = 
 {keyCode = 91
 ,name = "Windows"}

commandLeft = -- On the mac
 {keyCode = 91
 ,name = "Commandleft"}

commandRight = -- On the mac
 {keyCode = 93
 ,name = "Commandright"}

space = 
 {keyCode = 32
 ,name = "Space"}

enter = 
 {keyCode = 13
 ,name = "Enter"}

arrowRight = 
 {keyCode = 37
 ,name = "Arrowright"}

arrowLeft = 
 {keyCode = 39
 ,name = "Arrowleft"}

arrowUp = 
 {keyCode = 38
 ,name = "Arrowup"}

arrowDown = 
 {keyCode = 40
 ,name = "Arrowdown"}

backspace = 
 {keyCode = 8
 ,name = "Backspace"}

delete = 
 {keyCode = 46
 ,name = "Delete"}

insert = 
 {keyCode = 45
 ,name = "Insert"}

end = 
 {keyCode = 35
 ,name = "End"}

home = 
 {keyCode = 36
 ,name = "Home"}

pageDown = 
 {keyCode = 34
 ,name = "Pagedown"}

pageUp = 
 {keyCode = 33
 ,name = "Pageup"}

-- We don't define the F keys that are not availiable.  AKA, F1 is help, F3 is search.  F5 is refresh. Those keys cannot be used.

f2 = 
 {keyCode = 113
 ,name = "F2"}

f4 = 
 {keyCode = 115
 ,name = "F4"}

f8 = 
 {keyCode = 119
 ,name = "F8"}

f9 = 
 {keyCode = 120
 ,name = "F9"}

f10 = 
 {keyCode = 121
 ,name = "F10"}

one = 
 {keyCode = 49
 ,name = "1"}

two = 
 {keyCode = 50
 ,name = "2"}

three = 
 {keyCode = 51
 ,name = "3"}

four = 
 {keyCode = 52
 ,name = "4"}

five = 
 {keyCode = 53
 ,name = "5"}

six = 
 {keyCode = 54
 ,name = "6"}

seven = 
 {keyCode = 55
 ,name = "7"}

eight = 
 {keyCode = 56
 ,name = "8"}

nine = 
 {keyCode = 57
 ,name = "9"}

zero = 
 {keyCode = 58
 ,name = "0"}
{-
(defun insert-key (name keycode)
  "Insert a top level value declaration for a key"
  (interactive "MName:\nMKeycode:")
  (insert (concat name " = \n {keyCode = " keycode "\n ,name = \"" (capitalize name) "\"}\n\n")))-}