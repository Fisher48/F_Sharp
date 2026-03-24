let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)

// 50.2.1
let fac_seq = seq {
    let mutable i = 0
    while true do
        yield fact i
        i <- i + 1
}

// 50.2.2
let seq_seq = seq {
    for i in Seq.initInfinite id do
    if i % 2 = 0 then yield i / 2
    else yield -(i + 1) / 2
}


