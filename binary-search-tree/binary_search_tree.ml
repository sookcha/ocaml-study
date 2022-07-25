open Base

(* polymorphic & recursive type *)
type 'a bst = Empty | Node of { left : 'a bst; right : 'a bst; value : 'a }

let empty = Empty

let value = function
  | Empty -> Result.Error "현재값 비어있음"
  | Node { value; _ } -> Result.Ok value

let left = function
  | Empty -> Result.Error "왼쪽 비어있음"
  | Node { left; _ } -> Result.Ok left

let right = function
  | Empty -> Result.Error "오른쪽 비어있음"
  | Node { right; _ } -> Result.Ok right

let rec insert x = function
  (* 비어있는 트리라면 init *)
  | Empty -> Node { value = x; left = Empty; right = Empty }
  (* tree가 비어있지 않고 삽입하려는 값이 value보다 클 경우 오른쪽으로 insert *)
  | Node { value; left; right } when value < x ->
      Node { value; left; right = insert x right }
  (* tree가 비어있지 않고 삽입하려는 값이 value보다 작을 경우 왼쪽으로 insert *)
  | Node { value; left; right } when value >= x ->
      Node { value; left = insert x left; right }
  (* 이외의 케이스는 그대로 반환 *)
  | Node {value; left; right} -> Node {value; left; right}

let rec to_list = function
  (* 트리가 비어있으면 빈 리스트 *)
  | Empty -> []
  (* 그렇지 않다면 left + value + right *)
  | Node { value; left; right } -> (to_list left) @ [value] @ (to_list right)
