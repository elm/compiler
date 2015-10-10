
any isOk list =
  case list of
    first :: rest ->
        if isOk first then
            True

        else
            any isOk rest

    _ ->
        False