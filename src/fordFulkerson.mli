(* Implements the Ford-Fulkerson algorithm  *)

open Graph



(* find a path from the source node to the target node *)
val find_path: 'a graph -> id -> id -> 'a arc list

(*TODO create a ne graph from the find_path TODO*)
