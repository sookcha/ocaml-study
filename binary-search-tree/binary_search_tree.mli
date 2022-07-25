open Base

type 'a bst

val empty : 'a bst

val value : 'a bst -> ('a, string) Result.t

val left : 'a bst -> ('a bst, string) Result.t

val right : 'a bst -> ('a bst, string) Result.t

val insert : int -> int bst -> int bst

val to_list : 'a bst -> 'a list
