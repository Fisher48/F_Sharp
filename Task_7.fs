// 20.3.1
let vat (n: int) (x: float) : float = 
    if n <= 0 || n > 100 then x
    else x + (float (n) / 100.0) * x

// 20.3.2
let unvat (n: int) (x: float) : float = 
    if n <= 0 || n > 100 then x
    else x / (1.0 + float n / 100.0)

// 20.3.3
let rec min f =
    let rec find (n: int) : int =
        if f n = 0 then n
        else find(n + 1)
    find(1)


