// 16.1
let notDivisible (n,m) = m % n = 0

// 16.2
let prime n =
    let rec maxDivider d =
        match d with
        | 1 -> true
        | _ when n % d = 0 -> false
        | _ -> maxDivider(d - 1)
    match n with
    | _ when n < 2 -> false
    | _ -> maxDivider(n - 1)


