open Frontend
open! Cfg
open Apron

type t = Box.t Abstract1.t

let manager = Box.manager_alloc ()

let empty_env = Environment.make [||] [||]

(* let init v_l =
  let v_l' = List.map (fun v -> Var.of_string v) v_l |> Array.of_list in
  let env = Environment.make v_l' [||] in
  let expr = Linexpr1.make env in *)

let bottom = Abstract1.bottom manager empty_env

let assign env v expr =
  let expr = Texpr1.of_expr env (Texpr1.Cst (Coeff.s_of_int 2)) in
    

let join = Abstract1.join manager

let widen = Abstract1.widening manager

let subset = Abstract1.is_leq manager

let is_bottom = Abstract1.is_bottom manager

let print out = Abstract1.print (Format.formatter_of_out_channel out)
