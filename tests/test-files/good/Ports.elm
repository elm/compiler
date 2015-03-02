
-- incoming
input userID : String
input number : Int
input tuple : (Float,Bool)
input array : List Int
input record : { x:Float, y:Float }

-- outgoing
output fortyTwo : Int
output fortyTwo =
    42


output time : Float
output time =
    3.14

output students : List { name:String, age:Int }
output students =
    [ { name = "Tom", age = 42 } ]