// 34.1
let rec upto n =
    let rec upto_acc (m, acc) =
        if m = 1 then m :: acc
        else upto_acc(m - 1, m :: acc)
    if n < 1 then []
    else upto_acc(n, [])

// 34.2
let rec dnto n = match n with
    | 1 -> [1]
    | n when n <= 0 -> []
    | n when n > 1 -> n :: dnto(n - 1)

// 34.3
let rec evenn n =
    let rec evenn_acc (m, acc) =
        if m = 0 then m :: acc
        else evenn_acc (m - 2, m::acc)
    if n < 1 then []
    else evenn_acc(n * 2 - 2, [])


