open Base

let is_dna x = (Char.equal x 'A' || Char.equal x 'C' || Char.equal x 'G' || Char.equal x 'T')

let explode s = List.init (String.length s) ~f:(String.get s)
let is_dna_string x = 
  let filtered_length = String.filter x ~f:(fun x -> is_dna x)
  |> String.length in

  filtered_length = String.length x

let count_nucleotide s c =
  if not (is_dna c) then Result.Error 'X' 
  else if not (is_dna_string s) then Result.Error 'X' else
  let contains = String.filter s ~f:(fun x -> Char.equal x c)
    |> String.length in
  Result.Ok contains
  
let count_nucleotides s =
  if not (is_dna_string s) then Result.Error 'X' else
  let exploded = explode s in
  let count = List.map exploded ~f:(fun x -> (x, 1)) in
  let result = Map.of_alist_multi (module Char) count in
  let result2 = List.map (Map.to_alist result) ~f:(fun (a, b) -> (a, List.length b)) in
  Result.Ok (Map.of_alist_exn (module Char) result2)
  