let square = fun x -> x * x
let series_of_number number = List.init number (fun x -> x + 1)

let sum numbers = List.fold_left (+) 0 numbers

let square_of_sum number = 
    series_of_number number
    |> sum
    |> square

let sum_of_squares number =
    series_of_number number
    |> List.map (fun x -> square x)
    |> sum

let difference_of_squares number =
    square_of_sum number - sum_of_squares number
