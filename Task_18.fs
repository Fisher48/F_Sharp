// 47.4.1
let f n =
    let mutable i = 1
    let mutable result = 1
    while i <= n do
        result <- result * i
        i <- i+1
    result

// 47.4.2
let fibo n =
    let mutable i = 2
    let mutable sum = 0
    if n = 0 then 0
    elif n = 1 then 1
    else
    let mutable first = 0
    let mutable second = 1
    while i <= n do
        sum <- first + second
        first <- second
        second <- sum
        i <- i+1
    sum


