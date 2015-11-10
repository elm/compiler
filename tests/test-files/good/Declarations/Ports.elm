
-- incoming
port userID : String
port character : Char
port number : Int
port tuple : (Float,Bool)
port array : List Int
port record : { x:Float, y:Float }

-- outgoing
port fortyTwo : Int
port fortyTwo =
    42

port time : Float
port time =
    3.14

port letter : Char
port letter =
    'E'

port students : List { name:String, age:Int }
port students =
    [ { name = "Tom", age = 42 } ]