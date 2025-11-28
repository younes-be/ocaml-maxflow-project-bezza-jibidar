open Graph

let find_path graph src target = 
  let rec loop src acu = match (find_arc graph src target ) with
    |Some arc ->  (arc::acu)
    |None ->(
      let outarc_list = out_arcs graph src in 
      let rec loop2 arc_list acu =
          match arc_list with
            |[] -> []
            |x::rest -> let res = loop (x.tgt) (x::acu) in  if (res) = [] then loop2 rest acu else res

            in loop2 outarc_list acu )
          in
          loop src [] ;;
          


  
