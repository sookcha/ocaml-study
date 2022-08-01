type nucleotide = A | C | G | T


let diff dna1 dna2 =
  match dna1, dna2 with
  | (A, A) -> false
  | (C, C) -> false
  | (G, G) -> false
  | (T, T) -> false
  | _ -> true

let hamming_distance dna1 dna2 =
  if List.length dna1 = 0 && List.length dna2 > 0
    then Result.Error "left strand must not be empty"
  else if List.length dna2 = 0 && List.length dna1 > 0
    then Result.Error "right strand must not be empty"
  else if List.length dna1 <> List.length dna2
    then Result.Error "left and right strands must be of equal length"
  else
    let matched_count = List.map2 diff dna1 dna2
    |> List.filter (fun x -> x)
    |> List.length in
    Result.Ok matched_count
  
