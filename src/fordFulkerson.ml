open Graph
open Tools

let find_path graph src target = 
  
  (* On a du dupliquer, car quand il existe un arc entre notre src et la target dont le lbl =0 , l'algo doit quand même continuer la recherche de chemin et pas juste s'arrêter *)
  let rec loop src acu  visited = match (find_arc graph src target ) with
    |Some arc ->  
      if (arc.lbl=0) then 
        (
        let outarc_list = out_arcs graph src in 
        let rec loop21 arc_list acu =
          match arc_list with
          |[] -> []
          |x::rest -> 
            (*verification of an edge with a label = 0*)
            if (x.lbl = 0) then 
              loop21 rest acu 
            else 
            if List.mem x.tgt visited then 
              loop21 rest acu 
            else 
              let res = loop (x.tgt) (x::acu) (x.tgt :: visited) in  
              if (res) = [] then loop21 rest acu else res

        in loop21 outarc_list acu ) 
      else 
        (arc::acu)
    |None ->(
        let outarc_list = out_arcs graph src in 
        let rec loop2 arc_list acu =
          match arc_list with
          |[] -> []
          |x::rest -> 
            (*verification of an edge with a label = 0*)
            if (x.lbl = 0) then 
              loop2 rest acu 
            else 
            if List.mem x.tgt visited then 
              loop2 rest acu 
            else 
              let res = loop (x.tgt) (x::acu) (x.tgt :: visited) in  
              if (res) = [] then loop2 rest acu else res

        in loop2 outarc_list acu )
  in
  loop src [] [src] ;;



let flow_path arc_list =
  let rec loop list min = 
    match list with
    | [] -> min
    | x::rest -> if (x.lbl < min) then (loop rest x.lbl) else (loop rest min)
  in
  loop arc_list Int.max_int;;


let update_graph graph arc_list flow_n =
  let rec loop graph arc_list flow_n =
    match arc_list with 
    | [] -> graph
    | x::rest -> (let graph_bis = add_arc graph x.src x.tgt (-flow_n) in 
                  loop (add_arc graph_bis x.tgt x.src flow_n) rest flow_n) 
  in
  loop graph arc_list flow_n;;

let create_flow_graph base_graph ford_graph =
  let graph_out = clone_nodes base_graph in

  e_fold base_graph ( fun graph arc -> (
        match find_arc ford_graph arc.tgt arc.src with
        (* Si on trouve rien, ca veut dire que cette arc n'est pas mis a contribution*)
        |None -> new_arc graph {src =arc.src;tgt =arc.tgt;lbl = "0/"^(string_of_int arc.lbl)}
        |Some x -> (
            (*Si on trouve un arc , c'est que on a sûrement mis du flot sur cette arc  *)
            (*On check le flot sur x, si on a x.lbl > arc.lbl , c'est sûrement que dans le graphe de base , il existe un arc dasn les 2 sens*)
            (*et que x n'est pas vraiment mis à contribution*)
            (* if (x.lbl > arc.lbl) then (new_arc graph {src =arc.src;tgt =arc.tgt;lbl= "0/"^(string_of_int arc.lbl)}) 
            else (new_arc graph {src =arc.src;tgt=arc.tgt; lbl= (string_of_int x.lbl)^"/"^(string_of_int arc.lbl)}) *)

            match find_arc base_graph arc.tgt arc.src with
            |None -> new_arc graph {src=arc.src;tgt= arc.tgt;lbl= (string_of_int x.lbl)^"/"^(string_of_int arc.lbl)} 
            | Some arc_oppose ->(
              if x.lbl > arc_oppose.lbl then
                new_arc graph {src =arc.src;tgt=arc.tgt; lbl= (string_of_int x.lbl)^"/"^(string_of_int arc.lbl)}
              else
                new_arc graph {src =arc.src;tgt=arc.tgt; lbl= (string_of_int (arc_oppose.lbl-x.lbl))^"/"^(string_of_int arc_oppose.lbl)}
            )
          )    
      )
    ) graph_out

;;

let fordFurkerson graph src tgt =
  let rec loop graph src tgt =
    match (find_path graph src tgt) with
    |[] -> graph
    |path -> loop (update_graph graph path (flow_path path)) src tgt 
  in
  let finalGraph = loop graph src tgt in 
  create_flow_graph graph finalGraph 

;;

