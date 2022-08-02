type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let map_dna_to_rna = function
`A -> `U
| `C -> `G
| `G -> `C
| `T -> `A

let to_rna rnas =
    List.map map_dna_to_rna rnas
