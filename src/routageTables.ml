open Graph
open Tools
open FordFulkerson
(* Helper function to skip comments *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ -> graph

(* Reads a routeur header line: R ipRT idRouteur *)
let read_header_table graph line =
  try Scanf.sscanf line "R %s %d" (fun _ id -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "read_header_table"

(* Reads a routage link line: L ipRTi idRTi masque interface bandwith *)
(* Returns the destination node id and bandwidth *)
let read_routage_line line =
  try Scanf.sscanf line "L %s %d %s %s %d" (fun _ id _ _ bandwidth -> 
    Some (id, string_of_int bandwidth)
  )
  with e ->
    Printf.printf "Cannot read routage line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    None


let from_routage_tables_file path =

  let infile = open_in path in

  (* Read all lines from the file *)
  let rec read_all_lines acc =
    try
      let line = input_line infile in
      let line = String.trim line in
      read_all_lines (line :: acc)
    with End_of_file -> List.rev acc
  in

  let all_lines = read_all_lines [] in
  close_in infile ;

  (* First pass: create all nodes from 'R' lines *)
  let rec loop_nodes graph lines =
    match lines with
    | [] -> graph
    | line :: rest ->
        let graph2 =
          if line = "" then graph
          else match line.[0] with
            | 'R' -> read_header_table graph line
            | 'L' -> graph  (* Skip L lines in first pass *)
            | _ -> read_comment graph line
        in
        loop_nodes graph2 rest
  in

  let nodes_graph = loop_nodes empty_graph all_lines in

  (* Second pass: create arcs from 'L' lines, tracking the current router *)
  let rec loop_arcs graph current_router lines =
    match lines with
    | [] -> graph
    | line :: rest ->
        let (graph2, router2) =
          if line = "" then (graph, current_router)
          else match line.[0] with
            | 'R' ->
                (* Update current router source *)
                (try
                  Scanf.sscanf line "R %s %d" (fun _ id -> (graph, id))
                with _ -> (graph, current_router))
            | 'L' ->
                (* Add arc from current_router to destination with bandwidth as label *)
                (match read_routage_line line with
                  | Some (dest_id, bandwidth) ->
                      (new_arc graph { src = current_router; tgt = dest_id; lbl = bandwidth }, current_router)
                  | None -> (graph, current_router))
            | _ -> (read_comment graph line, current_router)
        in
        loop_arcs graph2 router2 rest
  in

  loop_arcs nodes_graph 0 all_lines
;;


let max_bandwith_routage filepath src dest =
  let strgraph = from_routage_tables_file filepath in 

   let graph =  gmap strgraph int_of_string in 
   let flowgraph =  fordFurkerson graph src dest in 
   flowgraph
;;