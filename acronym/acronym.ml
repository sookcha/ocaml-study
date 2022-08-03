let acronym message =
  message
  |> String.uppercase_ascii
  |> Str.split (Str.regexp {|[ -]+|})
  |> List.map (fun x -> Str.replace_first (Str.regexp "_") "" x)
  |> List.map (fun x -> String.get x 0)
  |> List.to_seq
  |> String.of_seq
