open Gfile
open Tools
open FordFulkerson
open Graph
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (*test de clone_nodes*)
  (*à cette étape, on a un graphe qui contient seulement les nodes du graphe dans le fichier.*)
  let test_cloned_graph = clone_nodes graph in

  (*on ajoute un arc qui n'existait pas dans le graphe précédent*)
  (*test de add_arc*)
  let test_int_graph = add_arc test_cloned_graph 1 2 67 in

  (*On passe d'un int graph à un string graphe*)
  (*test gmap*)
  let test_string_graph = gmap test_int_graph (string_of_int) in

  (*on réécrit le nouveau graphe (tous les noeuds et un seul arc de 1 à 2) dans un fichier*)
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile test_string_graph in

  (*On exporte sous le format graphviz*)
  (*test  de export*)
  let () = export (test_string_graph) ("./test_export.dot") in 
  (*optionnel : on peut créer un svg à partir du .dot*)
  (*avec la cmd: dot -Tsvg ./test_export.dot  > result.svg*)




  ()

(* Test pour FordFulkerson.find_path  sans boucle*)
let () =

  let graph = from_file "graphs/graph1.txt"
  in
  (* s'assurer qu'il y a un arc 1 -> 2 *)
  let path = find_path graph 0 5 in


  (* Afficher le résultat : la liste des arcs (src->tgt:lbl) *)
  let string_of_arc a = Printf.sprintf "%d->%d:%s" a.src a.tgt a.lbl in
  let path_str = String.concat " -> " (List.map string_of_arc (List .rev path)) in
  Printf.printf "find_path  : %s\n%!" (if path = [] then "(aucun chemin)" else path_str)



(* Test pour FordFulkerson.find_path  avec boucle*)
let () =

  let graphinit = from_file "graphs/graph1.txt"
  in

  (*rajouter une boucle avec une arête entre 5 et 2 *)
  let graph_boucle = add_arc (gmap graphinit int_of_string) 5 2 67 in
  (* s'assurer qu'il y a un arc 1 -> 2 *)
  let path = find_path (gmap graph_boucle string_of_int) 0 5 in

  (* Afficher le résultat : la liste des arcs (src->tgt:lbl) *)
  let string_of_arc a = Printf.sprintf "%d->%d:%s" a.src a.tgt a.lbl in
  let path_str = String.concat " -> " (List.map string_of_arc (List .rev path)) in
  Printf.printf "find_path  : %s\n%!" (if path = [] then "(aucun chemin)" else path_str)