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

let rec get_allergen_count x l =
    if x > 255 then get_allergen_count (x - 256) l
    else if Option.is_some @@ number_to_allergen x then l @ [number_to_allergen x]
    else if x <= 0 then l
    else let count = Int.ceil_log2 x in
    let allergen = (Int.pow 2 (count - 1)) in
    get_allergen_count (x - allergen) l @ [number_to_allergen allergen]

let allergic_to number allergen =
    let list = get_allergen_count number [] in 
    let result = List.map list ~f:(fun x -> Option.value x ~default:Cats) in
    let filtered = List.filter result ~f:(fun x -> x = allergen) in
    List.length filtered > 0

let allergies number =
    (* failwith "'allergic_to' is missing" *)
    let list = get_allergen_count number [] in 
    List.map list ~f:(fun x -> Option.value x ~default:Cats)
