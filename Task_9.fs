// 23.4.1
let get_result coppper_after =
    let gold = coppper_after / 240
    let silver = (coppper_after % 240) / 12
    let copper = coppper_after % 12
    (gold, silver, copper)

let (.+.) x y =
    let sum_copper (gold, silver, copper) = gold * 240 + silver * 12 + copper
    let coppper_after_sum = sum_copper x + sum_copper y
    get_result (coppper_after_sum)

let (.-.) x y =
    let sum_copper (gold, silver, copper) = gold * 240 + silver * 12 + copper
    let copper_after_subtraction = sum_copper x - sum_copper y
    get_result (copper_after_subtraction)


// 23.4.2
let (.+) (a: float, b: float) (c: float, d: float) = (a + c, b + d) 
let (.-) (a: float, b: float) (c: float, d: float) = (a - c, b - d)
let (.*) (a: float, b: float) (c: float, d: float) = (a * c - b * d, b * c + a * d)
let (./) (a: float, b: float) (c: float, d: float) = (a, b) .* (c / (c * c + d * d), -d / (c * c + d * d))


