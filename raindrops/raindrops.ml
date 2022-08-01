let make_3 number array =
    if number mod 3 = 0 then array @ ["Pling"] else array
let make_5 number array =
    if number mod 5 = 0 then array @ ["Plang"] else array
let make_7 number array =
    if number mod 7 = 0 then array @ ["Plong"] else array

let raindrop number =
    let result = 
        make_3 number []
        |> make_5 number
        |> make_7 number in
        
        if List.length result = 0 
            then Int.to_string number 
            else String.concat "" result