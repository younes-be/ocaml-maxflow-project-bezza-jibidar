open Gfile
open Tools
open FordFulkerson
open Graph
open RoutageTables

(* Test for gfile functions: from_file, write_file, export, gmap, clone_nodes, add_arc *)
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
  let () = export (test_string_graph) ("./test_export2.dot") in 
  (*optionnel : on peut créer un svg à partir du .dot*)
  (*avec la cmd: dot -Tsvg ./test_export.dot  > result.svg*)

  () ;;
(* test de findPath*)
let () =
  let strgraph = from_file "graphs/graphtestflow.txt" in 

  let graph =  gmap strgraph int_of_string in 
  let path =  find_path graph 0 3 in 
  let string_of_arc a = Printf.sprintf "%d->%d:%d" a.src a.tgt a.lbl in

  let path_str = String.concat " -> " (List.map string_of_arc (List .rev path)) in
  Printf.printf "find_path  : %s\n%!" (if path = [] then "(aucun chemin)" else path_str) in

();;


(* test de update graph et flow path*)
let () =
  let strgraph = from_file "graphs/graphtestflow.txt" in 

  let graph =  gmap strgraph int_of_string in 
  let path =  find_path graph 0 3 in 
  let resgraph = update_graph graph path (flow_path path) in 
  export (gmap resgraph string_of_int) "./resultflowpath.dot"  in
();;




(* Test for FordFulkerson*)
let () =
  (let strgraph = from_file "graphs/graphValue0.txt" in 

   let graph =  gmap strgraph int_of_string in 
   let flowgraph =  fordFurkerson graph 0 3 in 
   export flowgraph "./resultford.dot" ) in
();;

(* Test for FordFulkerson without cycle*)
let () =
  (let strgraph = from_file "graphs/graphtestflow.txt" in 

   let graph =  gmap strgraph int_of_string in 
   let flowgraph =  fordFurkerson graph 0 3 in 
   export flowgraph "./resulttestFord.dot" ) in
();;

(* Test for FordFulkerson with cycles*)
let () =
  (let strgraph = from_file "graphs/europeanGraph.txt" in 

   let graph =  gmap strgraph int_of_string in 
   let flowgraph =  fordFurkerson graph 0 3 in 
   export flowgraph "./resultEuroFord.dot" ) in
();;

(* Test for routage tables*)
let () =
  (let strgraph = from_routage_tables_file "graphs/routageTablesSimple.txt" in 

    
   export strgraph "./resultRoutage.dot" ) in
();;

(* Test for FordFulkerson with small routage Tables*)
let () =
  (let strgraph = from_routage_tables_file "graphs/routageTablesSimple.txt" in 

   let graph =  gmap strgraph int_of_string in 
   let flowgraph =  fordFurkerson graph 1 3 in 
   export flowgraph "./resultFordRoutage.dot" ;
   Printf.printf ("Pour l'exemple avec la table de routage simple, le débit max entre la source et la destination est : %d \n%!") (max_flow flowgraph 3)) in
();;

(* Test for FordFulkerson with medium routage Tables*)
let () =
  (let flowgraph = max_bandwith_routage "graphs/routageTablesMoyen.txt" 1 3 in
   export flowgraph "./resultFordBigRoutage.dot"   ;
  Printf.printf ("Pour l'exemple avec la table de routage medium, le débit max entre la source et la destination est : %d \n%!") (max_flow flowgraph 3))
  in ();;

(* Test for FordFulkerson with big routage Tables*)
let () =
  (let flowgraph = max_bandwith_routage "graphs/routageTablesGrand.txt" 1 3 in
   export flowgraph "./resultFordBigRoutage.dot" ;
   Printf.printf ("Pour l'exemple avec la grande table de routage , le débit max entre la source et la destination est : %d \n%!") (max_flow flowgraph 3)) in
();;
