(* Implements the routage algorithm  *)

open Gfile
open Graph


(* Values are read as strings. *)

val from_routage_tables_file: path -> string graph

(* Computes the maximum bandwith between two nodes in a routage table graph *)
val max_bandwith_routage: path -> int -> int -> string graph