(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(*Create a new graph with all nodes and without the edges from a graph*)
let clone_nodes gr =  n_fold gr new_node empty_graph ;;

let gmap gr f = 

  let newgr = clone_nodes gr in 
  e_fold gr (fun newgr aarc -> new_arc newgr {src = aarc.src ; tgt = (aarc.tgt) ; lbl = (f aarc.lbl) }) newgr;;


let add_arc g id1 id2 n = 
  match find_arc g id1 id2 with
  |None -> new_arc g {src = id1 ; tgt = (id2) ; lbl = (n) }
  |Some arc -> new_arc g {src = id1 ; tgt = id2 ; lbl = (arc.lbl+n)} ;;




