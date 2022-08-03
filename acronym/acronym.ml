open Base

let acronym message =
  message
  |> String.uppercase
  |> String.split_on_chars ~on:[' '; '_'; '-']
  |> List.filter ~f:(fun x -> not(String.equal "" x))
  |> List.map ~f:(fun x -> String.get x 0)
  |> String.of_char_list
