import Http

makeUrl : String -> Http.Request String
makeUrl tag =
    let url = "http://api.flickr.com/services/rest/?format=json" ++
        "&nojsoncallback=1&api_key=256663858aa10e52a838a58b7866d858" ++
        "&method=flickr.photos.search&sort=random&per_page=10&tags="
    in  Http.get (url ++ tag)

pictures : Signal (Http.Response String)
pictures = Http.send (constant (makeUrl "hello"))

test : Http.Response String -> Bool
test response = case response of
    Http.Waiting -> True
    Http.Failure code message headers -> False
    Http.Success message headers -> True

main = asText (foldp (&&) True (test <~ pictures))
