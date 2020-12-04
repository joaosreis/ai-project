open Apron
open Frontend.Abstract_syntax_tree

let binop = function
  | AST_PLUS -> Texpr1.Add
  | AST_MINUS -> Texpr1.Sub
  | AST_MULTIPLY -> Texpr1.Mod
  | AST_DIVIDE -> Texpr1.Div
  | AST_MODULO -> Texpr1.Mod

let rec texpr_of_int_expr =
  let open Frontend.Cfg in
  function
  | CFG_int_var v -> Texpr1.Var (Apron.Var.of_string v.var_name)
  | CFG_int_const i ->
      let i' = Mpqf.of_string (Z.to_string i) in
      Texpr1.Cst (Coeff.s_of_mpqf i')
  | CFG_int_rand (i_1, i_2) ->
      let i_1' = Mpqf.of_string (Z.to_string i_1) in
      let i_2' = Mpqf.of_string (Z.to_string i_2) in
      Texpr1.Cst (Coeff.i_of_mpqf i_1' i_2')
  | CFG_int_unary (op, expr) -> (
      match op with
      | AST_UNARY_PLUS -> texpr_of_int_expr expr
      | AST_UNARY_MINUS ->
          Texpr1.Unop
            (Texpr1.Neg, texpr_of_int_expr expr, Texpr1.Int, Texpr1.Near))
  | CFG_int_binary (op, expr_1, expr_2) ->
      let expr_1' = texpr_of_int_expr expr_1 in
      let expr_2' = texpr_of_int_expr expr_2 in
      Texpr1.Binop (binop op, expr_1', expr_2', Texpr1.Int, Texpr1.Near)

let negate_texpr texpr =
  let expr = Apron.Texpr1.to_expr texpr in
  let nexpr =
    match expr with
    | Apron.Texpr1.Unop (Apron.Texpr1.Neg, e, _, _) -> e
    | _ ->
        Apron.Texpr1.Unop
          (Apron.Texpr1.Neg, expr, Apron.Texpr1.Real, Apron.Texpr1.Rnd)
  in
  let env = Apron.Texpr1.get_env texpr in
  Apron.Texpr1.of_expr env nexpr

let negate_tcons tcons =
  let texpr = Apron.Tcons1.get_texpr1 tcons in
  let ntyp, ntexpr =
    match Apron.Tcons1.get_typ tcons with
    | Apron.Tcons1.EQ -> (Apron.Tcons1.DISEQ, texpr)
    | Apron.Tcons1.DISEQ -> (Apron.Tcons1.EQ, texpr)
    | Apron.Tcons1.SUPEQ -> (Apron.Tcons1.SUP, negate_texpr texpr)
    | Apron.Tcons1.SUP -> (Apron.Tcons1.SUPEQ, negate_texpr texpr)
    | Apron.Tcons1.EQMOD _ -> failwith "EQMOD not supported now"
  in
  Tcons1.make ntexpr ntyp

let compare_op expr_1 expr_2 = function
  | AST_EQUAL -> (Tcons1.EQ, expr_1, expr_2)
  | AST_GREATER -> (Tcons1.SUP, expr_1, expr_2)
  | AST_GREATER_EQUAL -> (Tcons1.SUPEQ, expr_1, expr_2)
  | AST_LESS -> (Tcons1.SUP, expr_2, expr_1)
  | AST_LESS_EQUAL -> (Tcons1.SUPEQ, expr_2, expr_1)
  | AST_NOT_EQUAL -> (Tcons1.DISEQ, expr_1, expr_2)

let tcons_of_compare env op expr_1 expr_2 =
  let op', expr_1', expr_2' = compare_op expr_1 expr_2 op in
  let expr_1'' = texpr_of_int_expr expr_1' in
  let expr_2'' = texpr_of_int_expr expr_2' in
  let expr =
    Texpr1.Binop (Texpr1.Sub, expr_1'', expr_2'', Texpr1.Int, Texpr1.Near)
  in
  Tcons1.make (Texpr1.of_expr env expr) op'

let boolexpr0_of_bool_expr env =
  let cand t1 t2 = Boolexpr.make_conjunction (Array.append t1 t2) in
  let rec translate =
    let open Frontend.Cfg in
    function
    | CFG_bool_const b -> Boolexpr.make_cst b
    | CFG_bool_rand -> Boolexpr.make_cst true
    | CFG_compare (op, expr_1, expr_2) ->
        let tcons = tcons_of_compare env op expr_1 expr_2 in
        Boolexpr.make_conjunction [| tcons |]
    | CFG_bool_binary (op, expr_1, expr_2) -> (
        match op with
        | AST_AND ->
            Boolexpr.make_and ~cand (translate expr_1) (translate expr_2)
        | AST_OR -> Boolexpr.make_or (translate expr_1) (translate expr_2))
    | CFG_bool_unary (AST_NOT, expr) -> (
        match expr with
        | CFG_bool_const false | CFG_bool_rand -> Boolexpr.make_cst true
        | CFG_bool_const true -> Boolexpr.make_cst false
        | CFG_compare (op, expr_1, expr_2) ->
            let tcons = tcons_of_compare env op expr_1 expr_2 in
            let tcons = negate_tcons tcons in
            Boolexpr.make_conjunction [| tcons |]
        | CFG_bool_binary (op, expr_1, expr_2) -> (
            let make_not e = CFG_bool_unary (AST_NOT, e) in
            match op with
            | AST_AND ->
                Boolexpr.make_or
                  (translate (make_not expr_1))
                  (translate (make_not expr_2))
            | AST_OR ->
                Boolexpr.make_and ~cand
                  (translate (make_not expr_1))
                  (translate (make_not expr_2)))
        | CFG_bool_unary (AST_NOT, expr) -> translate expr)
  in
  translate

let boolexpr_of_bool_expr env expr =
  let bexpr0 = boolexpr0_of_bool_expr env expr in
  Boolexpr.map
    (fun tcons ->
      assert (tcons <> [||]);
      let res = Apron.Tcons1.array_make env (Array.length tcons) in
      Array.iteri (fun i cons -> Apron.Tcons1.array_set res i cons) tcons;
      res)
    bexpr0
