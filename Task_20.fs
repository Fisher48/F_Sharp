// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2 * (i+1))

// 49.5.2
let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)

let fac_seq = Seq.initInfinite (fun i -> fact i)

// 49.5.3
let seq_seq = Seq.initInfinite(fun i -> if i % 2 = 0 then i/2 else -(i+1)/2)


