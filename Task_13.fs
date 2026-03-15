// 39.1
let rec rmodd = function 
    | [] | [_] -> []
    | _ :: second :: tail -> second :: rmodd tail   

// 39.2
let rec del_even = function
    | [] -> []
    | head :: tail -> 
        if head % 2 = 0 then del_even tail
        else head :: del_even tail

// 39.3
let rec multiplicity x xs = 
    let rec helpMult lst acc = 
        match lst with
        | [] -> acc 
        | head :: tail -> 
            if head = x then helpMult tail acc + 1
            else helpMult tail acc
    helpMult xs 0

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x :: y :: tail -> 
        let (first, second) = split tail 
        (x :: first, y :: second)

// 39.5
let rec zip = function
    | [], [] -> []
    | x :: xs, y :: ys -> (x, y) :: zip (xs, ys)
    | _ -> failwith "Списки разной длины"


