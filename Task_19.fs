// 48.4.1
let rec fibo1 n n1 n2 = 
    match n with
    | 0 -> n2
    | _ -> fibo1 (n-1) (n2+n1) n1

// 48.4.2
let rec fibo2 n c = 
    if n <= 1 then c n
    else fibo2 (n-1) (fun x -> fibo2 (n-2) (fun y -> c (x + y)))

// 48.4.3
let rec bigList n k =
    let acc = k []
    match n with
    | 0 -> acc
    | _ -> bigList (n-1) (fun _ -> 1 :: acc)


