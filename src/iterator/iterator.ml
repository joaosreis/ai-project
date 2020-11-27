module Iterator (D : Domains.Domain.DOMAIN) = struct
  type abtract_values = D.t Frontend.Cfg.VarMap.t

  let traverse
      (abstract_i : abtract_values -> Frontend.Cfg.inst -> abtract_values) c =
    let open Frontend.Cfg in
    let node_value_map =
      List.fold_left
        (fun acc n ->
          let var_map =
            List.fold_left
              (fun acc v -> VarMap.add v D.bottom acc)
              VarMap.empty c.cfg_vars
          in
          NodeMap.add n var_map acc)
        NodeMap.empty c.cfg_nodes
    in
    let wl = c.cfg_nodes in
    let successor_nodes n = List.map (fun arc -> arc.arc_dst) n.node_out in
    let rec traverse_aux node_value_map = function
      | [] -> node_value_map
      | n :: wl -> (
          (* Apply the abstract instruction to each predecessor arc *)
          let abstract_values_pred_list =
            List.map
              (fun arc ->
                let abstract_values = NodeMap.find arc.arc_src node_value_map in
                abstract_i abstract_values arc.arc_inst)
              n.node_in
          in
          match abstract_values_pred_list with
          | [] -> traverse_aux node_value_map wl
          | hd :: tl ->
              (* Join the result of each predecessecor arc *)
              let abstract_values' =
                List.fold_left
                  (VarMap.map2 (fun _ v_1 v_2 -> D.join v_1 v_2))
                  hd tl
              in
              let abstract_values = NodeMap.find n node_value_map in
              (* TODO: widening *)
              let node_value_map' =
                NodeMap.add n abstract_values' node_value_map
              in
              (* Check if values changed *)
              let b =
                VarMap.fold2
                  (fun _ v_1 v_2 acc -> acc || v_1 = v_2)
                  abstract_values abstract_values' false
              in
              (* Add successor node to the list if the values changed *)
              let wl' = if b then wl @ successor_nodes n else wl in
              traverse_aux node_value_map' wl')
    in
    traverse_aux node_value_map wl
end
