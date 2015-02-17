
-- incoming
input userID : String
input signal : Signal Int
input tuple : (Float,Bool)
input array : List Int
input record : { x:Float, y:Float }

-- outgoing
output fortyTwo : Int
output fortyTwo = 42

output time : Signal Float
output time = every second

output students : Signal (List {name:String, age:Int})
output students = constant []