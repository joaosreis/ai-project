open Frontend
open! Cfg
open Apron

module Make (M : sig
  type t

  val man : t Manager.t
end) : Domain.DOMAIN = struct
  type t = M.t Abstract1.t

  let man = M.man

  let empty_env = Environment.make [||] [||]

  let init v_l =
    let v_l' =
      List.map (fun v -> Var.of_string v.var_name) v_l |> Array.of_list
    in
    let env = Environment.make v_l' [||] in
    let earray = Tcons1.array_make env (Array.length v_l') in
    Abstract1.of_tcons_array man env earray

  let bottom = Abstract1.bottom man empty_env

  let guard abs expr =
    let env = abs.Abstract1.env in
    let boolexpr = Ast_to_apron.boolexpr_of_bool_expr env expr in
    let labstract =
      match boolexpr with
      | Boolexpr.TRUE -> [ abs ]
      | Boolexpr.DISJ lconj ->
          List.map (fun conj -> Abstract1.meet_tcons_array man abs conj) lconj
    in
    match labstract with
    | [] -> Abstract1.bottom man env
    | [ x ] -> x
    | _ -> Abstract1.join_array man (Array.of_list labstract)

  let assign a v expr =
    let texpr =
      Texpr1.of_expr a.Abstract1.env (Ast_to_apron.texpr_of_int_expr expr)
    in
    Abstract1.assign_texpr man a (Var.of_string v.var_name) texpr None

  let join = Abstract1.join man

  let widen = Abstract1.widening man

  let subset = Abstract1.is_leq man

  let is_bottom = Abstract1.is_bottom man

  let print out = Abstract1.print (Format.formatter_of_out_channel out)
end
