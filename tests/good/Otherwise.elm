module Otherwise where

otherwise = 3

main = asText <| if False then "Yay" else "Boo"
