open Base

type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
let (=) = Poly.(=)


let number_to_allergen = function
1 -> Some(Eggs)
| 2 -> Some(Peanuts)
| 4 -> Some(Shellfish)
| 8 -> Some(Strawberries)
| 16 -> Some(Tomatoes)
| 32 -> Some(Chocolate)
| 64 -> Some(Pollen)
| 128 -> Some(Cats)
| _ -> None    

(* 알러지 수치는 2의 거듭제곱 꼴이다. 이 속성을 활용하여 문제 해결.
   정수에 log_2 를 먹여서 2의 지수로 변환 -> 내가 가질 수 있는 알러지의 최대치 + 1이 나옴
   내가 가질 수 있는 알러지의 최대치는 -> let max_allergen = 2^(ceil(log_2 x) - 1)
   해당 알러지를 획득한다. (내 현재 알러지 수치 - 획득한 알러지 수치) 이 식을 계산하면 또 얻을 수 있는 알러지 수치가 나온다.
   그러면 얻을 수 있는 알러지 수치에 대해서 위 과정 반복.
   언젠가는 내가 가질 수 있는 알러지의 최대치가 0 이하가 될 것이다. 그것이 종료 조건.*)
let rec get_allergen_count ?(l = []) x =
    if x > 255 then get_allergen_count (x - 256) ~l
    else if Option.is_some @@ number_to_allergen x then [Option.value_exn (number_to_allergen x)] @ l
    else if x <= 0 then l
    else let count = Int.ceil_log2 x in
    let allergen = Int.pow 2 (count - 1) in
    get_allergen_count (x - allergen) ~l:[Option.value_exn (number_to_allergen allergen)] @ l

let allergic_to number allergen =
    get_allergen_count number
    |> List.exists ~f:(fun x -> x = allergen)
    
let allergies number =
    get_allergen_count number
