(* Implements the Ford-Fulkerson algorithm  *)

open Graph



(* find a path from the source node to the target node *)
val find_path: int graph -> id -> id -> int arc list

(* Find the max flow we can put on the path*)
val flow_path: int arc list -> int

(*TODO create a new graph from the find_path TODO*)
val update_graph : int graph -> int arc list -> int -> int graph

val create_flow_graph :int graph -> int graph -> string graph

val fordFurkerson : int graph -> id -> id -> string graph
