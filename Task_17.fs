// 43.3
let try_find key m =
    match key with
    | key when Map.containsKey key m = false -> None
    | key -> Some(Map.find key m)


