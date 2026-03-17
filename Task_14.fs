// 40.1
let rec sum (p, xs) =
    match xs with
    | ([]) -> 0
    | head :: tail ->
        let rest = sum (p, tail)
        if p head
            then head + rest
        else rest

// 40.2.1
let rec count (xs, n) =
    let rec helpCount (lst, num, acc) =
        match lst with
        | ([]) -> acc
        | head :: tail ->
            if head = n then helpCount (tail, n, acc + 1)
            else helpCount (tail, n, acc)
    helpCount (xs, n, 0)

// 40.2.2
let rec insert (xs, n) =
    match xs with
    | [] -> [n]
    | head :: tail ->
        if n <= head
            then n :: xs
        else head :: insert (tail, n)

// 40.2.3
let rec intersect (xs1, xs2) =
    match xs1, xs2 with
    | [], _ | _, []  -> []
    | head1 :: tail1, head2 :: tail2 ->
        if head1 = head2
            then head1 :: intersect (tail1, tail2)
        elif head1 > head2
            then intersect (xs1, tail2)
        else intersect (tail1, xs2)

// 40.2.4
let rec plus (xs1, xs2) =
    match xs1, xs2 with
    | [], [] -> []
    | [], xs2 -> xs2
    | xs1, [] -> xs1
    | head1 :: tail1, head2 :: tail2 ->
        if head1 > head2
            then head2 :: plus (xs1, tail2)
        elif head1 < head2
            then head1 :: plus (tail1, xs2)
        else head1 :: plus (tail1, tail2)

// 40.2.5
let rec minus (xs1, xs2) =
    match xs1, xs2 with
    | [], [] -> []
    | [], xs2 -> []
    | xs1, [] -> xs1
    | h1 :: t1, h2 :: t2 ->
        if h1 < h2
            then h1 :: minus (t1, xs2)
        elif h1 > h2
            then minus (xs1, t2)
        else minus (t1, t2)

// 40.3.1
let rec smallest = function
    | [] -> None
    | [x] -> Some(x)
    | first :: second :: tail ->
        if first < second then smallest (first :: tail)
        else smallest (second :: tail)

// 40.3.2
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | head :: tail ->
    if head = n then tail
    else head :: delete (n, tail)

// 40.3.3
let rec sort lst =
    let min = smallest lst
    match min with
    | None -> []
    | Some min -> min :: sort (delete (min, lst))

// 40.4
let rec revrev lst =
    let rec reverse list_of_lists =
        match list_of_lists with
        | [] -> []
        | head :: tail -> List.rev head :: reverse tail
    List.rev (reverse lst)


