type TimeOfDay = { hours: int; minutes: int; f: string }

let to24Hour t =
    match t.f with
    | "AM" -> if t.hours = 12 then 0 else t.hours
    | "PM" -> if t.hours = 12 then 12 else t.hours + 12
    | _ -> failwith "Неверный формат времени"

let (.>.) (x: TimeOfDay) (y: TimeOfDay) =
    let x_24_hours = to24Hour x
    let y_24_hours = to24Hour y
    if x_24_hours > y_24_hours then true
    elif x_24_hours < y_24_hours then false
    else x.minutes > y.minutes


