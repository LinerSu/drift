open Syntax
open AbstractDomain
open SemanticDomain
open SemanticsDomain
open SensitiveDomain
open SenSemantics

(** Pretty printing *)
let pr_relation ppf = function
  | Bool (vt, vf) -> Format.fprintf ppf "@[<1>{@ cur_v:@ Bool@ |@ TRUE:@ %a,@ FALSE:@ %a@ }@]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
  | Int v -> Format.fprintf ppf "@[<1>{@ cur_v:@ Int@ |@ %a@ }@]" AbstractValue.print_abs v
  | Unit u -> Format.fprintf ppf "@[<1>Unit@]"

let pr_label pl ppf l = if pl then Format.fprintf ppf "^%s" l else ()

let pr_const ppf value = Format.fprintf ppf "%s" (str_of_val value)

let pr_op ppf op = Format.fprintf ppf "%s" (string_of_op op)

let pr_unop ppf op = Format.fprintf ppf "%s" (string_of_unop op)

let rec pr_exp pl ppf = function
| TupleLst (u, l) -> 
    let rec print_tuple pl ppf = function
      | [] -> ()
      | [e] -> Format.fprintf ppf "@[<1>%a@]" 
        (pr_exp pl) e
      | e :: tl -> 
        Format.fprintf ppf "@[<2>%a,@ %a@]" 
        (pr_exp pl) e
        (print_tuple pl) tl
    in
    Format.fprintf ppf "@[<2>(%a)%a@]"
      (print_tuple pl) u (pr_label pl) l
| Const (c, l) ->
    Format.fprintf ppf "%a%a" pr_const c (pr_label pl) l
| Var (x, l) ->
    Format.fprintf ppf "%s%a" x (pr_label pl) l
| App (e1, e2, l) ->
    Format.fprintf ppf "@[<2>(%a@ %a)%a@]"
      (pr_exp pl) e1
      (pr_exp pl) e2
      (pr_label pl) l
| Rec (None, (x, lx), e, l) ->
    Format.fprintf ppf "@[<3>(lambda %s%a.@ %a)%a@]"
      x (pr_label pl) lx
      (pr_exp pl) e
      (pr_label pl) l
| Rec (Some (f, lf), (x, lx), e, l) ->
    Format.fprintf ppf "@[<3>(mu %s%a %s%a.@ %a)%a@]"
      f (pr_label pl) lf
      x (pr_label pl) lx
      (pr_exp pl) e
      (pr_label pl) l
| Ite (e1, e2, e3, l, _) ->
    Format.fprintf ppf "@[<2>(%a@ ?@ %a@ :@ %a)%a@]"
      (pr_exp pl) e1
      (pr_exp pl) e2
      (pr_exp pl) e3
      (pr_label pl) l
| BinOp (bop, e1, e2, l) ->
    Format.fprintf ppf "@[<3>(%a@ %a@ %a)%a@]"
    (pr_exp pl) e1
    pr_op bop
    (pr_exp pl) e2
    (pr_label pl) l
| UnOp (uop, e1, l) ->
    Format.fprintf ppf "@[<3>(%a@ %a)%a@]"
    pr_unop uop
    (pr_exp pl) e1
    (pr_label pl) l
| PatMat (e, patlst, l) ->
  Format.fprintf ppf "@[<2>(match@ %a@ with@ %a)%a@]"
    (pr_exp pl) e
    (pr_pm pl) patlst
    (pr_label pl) l
| DataTyp (name, tag, ep, l) ->
  (match ep with
  | Some e -> Format.fprintf ppf "@[<3>(%s@ %a)%a@]"
    tag (pr_exp pl) e
    (pr_label pl) l
  | None -> Format.fprintf ppf "@[<2>(%s)%a@]"
    tag (pr_label pl) l)
| Ptr (e, l) -> Format.fprintf ppf "@[<2>(ref %a)%a@]"
  (pr_exp pl) e
  (pr_label pl) l
| DeRef (e, l) -> Format.fprintf ppf "@[<2>(* %a)%a@]"
  (pr_exp pl) e
  (pr_label pl) l
and pr_pm pl ppf = function
  | [] -> ()
  | [c] -> Format.fprintf ppf "%a" (pr_pattern pl) c
  | hd::tl -> Format.fprintf ppf "%a@ |@ %a" (pr_pattern pl) hd (pr_pm pl) tl
and pr_pattern pl ppf (Case (e1, e2)) = 
  Format.fprintf ppf "@[<2>%a@ ->@ %a@]" (pr_exp pl) e1 (pr_exp pl) e2

let print_exp out_ch e = Format.fprintf (Format.formatter_of_out_channel out_ch) "%a@?" (pr_exp true) e

let loc_of_node n = get_label_snode n

let pr_node ppf n = Format.fprintf ppf "%s" (loc_of_node n)

let rec pr_node_full ppf n = print_node n ppf pr_env
and pr_env ppf = function
  | [] -> ()
  | [x, (n,r)] -> Format.fprintf ppf "%s, %b: %a" x r pr_node_full n
  | (x, (n,r)) :: env -> Format.fprintf ppf "%s, %b: %a,@ %a" x r pr_node_full n pr_env env

let string_of_node n = pr_node Format.str_formatter n; Format.flush_str_formatter ()

let pr_agg_val ppf a = match a with
  | Bool (vt, vf) -> Format.fprintf ppf "@[<1>@ TRUE:@ %a,@ FALSE:@ %a@ @]"  AbstractValue.print_abs vt AbstractValue.print_abs vf
  | Int v -> Format.fprintf ppf "@[<1>@ %a@ @]" AbstractValue.print_abs v
  | Unit u -> Format.fprintf ppf "@[<1>Unit@]"

let pr_ary ppf ary = 
  let (l,e), (rl, ve) = ary in
  Format.fprintf ppf "@[<1>{@ cur_v:@ Int Array (%s, %s)@ |@ len:@ %a,@ item:@ %a@ }@]" l e pr_agg_val rl pr_agg_val ve

let string_of_ownership own = 
  if own = 0.0 then "Ref^0"
  else if own = 1.0 then "Ref^1"
  else Format.sprintf "Ref^%.1f" own

let print_nothing ppf _ = Format.fprintf ppf ""

let rec shape_value = function
  | Bot -> "Bot"
  | Top -> "Top"
  | Relation r -> (match r with
    | Int _ -> "Int"
    | Bool _ -> "Bool"
    | Unit _ -> "Unit")
  | Table t -> let (_, (vi,vo)) = get_full_table_T t in 
    (shape_value vi)^"->"^(shape_value vo)
  | Tuple u -> if List.length u = 0 then "Unit"
    else 
      let rec shape_tuple = function
      | [] -> ""
      | [it] -> shape_value it
      | hd :: tl -> (shape_value hd) ^ "*" ^ (shape_tuple tl) in
      shape_tuple u
  | Ary _ -> "Array"
  | Lst _ -> "List"
  | Variant (tag, _) -> tag
  | Ref (ref, own) -> string_of_ownership own

let rec pr_value_name ppf= function
  | _, Bot -> Format.fprintf ppf "_|_"
  | _, Top -> Format.fprintf ppf "T"
  | dep_name, Relation r -> pr_relation_name ppf (dep_name, r)
  | _ -> ()
and pr_relation_name ppf = function
  | dep_name, Bool (vt, vf) -> Format.fprintf ppf "@[<1>{@ %s:@ Bool@ |@ TRUE:@ %a,@ FALSE:@ %a@ }@]" dep_name AbstractValue.print_abs vt AbstractValue.print_abs vf
  | dep_name, Int v -> Format.fprintf ppf "@[<1>{@ %s:@ Int@ |@ %a@ }@]" dep_name AbstractValue.print_abs v
  | dep_name, Unit u -> Format.fprintf ppf "@[<1>Unit@]"

let rec pr_value ppf v = match v with
  | Bot -> Format.fprintf ppf "_|_"
  | Top -> Format.fprintf ppf "T"
  | Relation r -> pr_relation ppf r
  | Table t -> print_table t ppf pr_value
  | Tuple u -> pr_tuple ppf u
  | Ary ary -> pr_ary ppf ary
  | Lst lst -> pr_lst ppf lst
  | Variant v -> pr_variant ppf v
  | Ref ref -> pr_ref ppf ref
and pr_lst ppf lst =
    let (l,e), (rl, ve) = lst in
    Format.fprintf ppf "@[<1>{@ cur_v:@ %s List (%s, %s)@ |@ len:@ %a,@ item:@ %a@ }@]" (shape_value ve) l e pr_agg_val rl pr_value ve
and pr_tuple ppf u = 
  if List.length u = 0 then Format.fprintf ppf "@[<1>Unit@]"
  else 
    let rec print_list ppf = function
    | [] -> ()
    | [it] -> pr_value ppf it
    | hd :: tl -> 
    Format.fprintf ppf "@[<1>%a,@ %a @]" pr_value hd print_list tl in
    print_list ppf u
and pr_variant_tuple ppf u = 
  let idx = ref 0 in
  let rec print_list ppf = function
    | [] -> ()
    | [it] -> let name, dp = get_var_dep !idx in
      Format.fprintf ppf "@[<1> %s %a @]" name pr_value_name (dp, it)
    | hd :: tl -> let name, dp = get_var_dep !idx in
      idx := !idx + 1 ;
      Format.fprintf ppf "@[<1> %s %a,@ %a @]" name pr_value_name (dp, hd) print_list tl
  in
  print_list ppf u
and pr_variant ppf v = 
  let tag, u = v in
  let name, _ = find_dtname_by_tag tag in
  Format.fprintf ppf "@[<1>{@ cur_v:@ %s@ %s@ | " name tag;
    if List.length u = 0 then Format.fprintf ppf "@ }@]" else
    Format.fprintf ppf "@ %a }@]" pr_variant_tuple u
and pr_ref ppf (v, own) = Format.fprintf ppf "%a@ %s@ " pr_value v (string_of_ownership own)


let sort_list (m: exec_map_t) =
  let comp s1 s2 =
  let l1 = try int_of_string s1 
    with _ -> -1 in
  let l2 = try int_of_string s2
    with _ -> -1 in
  if l1 = -1 then
    if l2 = -1 then String.compare s1 s2
    else -1
  else if l2 = -1 then 1
  else l1 - l2 in
  let lst = (NodeMap.bindings m) in
  List.sort (fun (n1,_) (n2,_) -> 
    compare_node comp n1 n2) lst 

let print_value out_ch v = Format.fprintf (Format.formatter_of_out_channel out_ch) "%a@?" pr_value v

let string_of_value v = pr_value Format.str_formatter v; Format.flush_str_formatter ()

let rec pr_exec_map ppf m =
  Format.fprintf ppf "----\n%a----\n" pr_exec_rows (sort_list m)
and pr_exec_rows ppf = function
  | [] -> ()
  | [row] -> Format.fprintf ppf "%a\n" pr_exec_row row
  | row :: rows -> Format.fprintf ppf "%a@\n@\n%a" pr_exec_row row pr_exec_rows rows
and pr_exec_row ppf (n, v) =
  Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]" pr_node n pr_value v

let print_exec_map m = Format.fprintf Format.std_formatter "%a@?" pr_exec_map m

let rec print_exps out_ch = function
| [] -> ()
| e :: tl -> print_exp out_ch e; print_exps out_ch tl

let rec str_toplist str = function 
| [] -> str
| [(vr,ty)] -> str^"("^vr^", "^(str_of_type ty)^")"
| (vr,ty)::l -> let s' = str^"("^vr^", "^(str_of_type ty)^"); " in (str_toplist s' l)

let pr_pre_exp ppf = function
  | {name = n; dtype = d; left = l; right = r} -> 
    if l = "top" then Format.fprintf ppf "{%s:%s | %s}" n (type_to_string d) l
    else Format.fprintf ppf "{%s:%s | %s = %s}" n (type_to_string d) l r

let pr_pre_def_vars ppf = 
  let rec pr_ll = function
  | [] -> ()
  | (k, ls)::tl -> Format.fprintf ppf "@[<2>%s@ ->@ %a@]\n" k pr_pre_exp ls; pr_ll tl
  in
  let ls = VarDefMap.bindings !pre_vars in
  pr_ll ls

let print_last_node m = 
  let pr_map_lst_node ppf m = let lst = (sort_list m) in 
    let lst_n, v = List.nth lst (List.length lst - 1) in
    Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]\n\n" pr_node lst_n pr_value v
  in
  Format.fprintf Format.std_formatter "%a@?" pr_map_lst_node m

let print_node_by_label m l = 
  let pr_map_nst_node ppf m = let lst = (sort_list m) in
  try
    let nst_n, v = List.nth lst (l + 4) in
    Format.fprintf ppf "@[<2>%a |->@ @[<2>%a@]@]\n\n" pr_node nst_n pr_value v
  with Failure s -> ()
  in
  Format.fprintf Format.std_formatter "%a@?" pr_map_nst_node m

let print_pred ppf = function
  | True -> Format.fprintf ppf "True" 
  | False -> Format.fprintf ppf "True" 
  | Exp exp -> 
    let rec print_mul_term ppf = function
    | Single t -> pr_exp true ppf t
    | And (t, lst) -> pr_exp true ppf t; Format.fprintf ppf " and "; print_mul_term ppf lst
    | Or (t, lst) -> pr_exp true ppf t; Format.fprintf ppf " or "; print_mul_term ppf lst
    in print_mul_term ppf exp

let print_type_pred ppf pred = 
  VarMap.iter (fun k v -> 
    let name_var, dp_var, type_ref, pred = v in
    Format.fprintf ppf "%s: {%s: %s | %a }" name_var dp_var type_ref print_pred pred) pred

let rec print_type_spec ppf = function
  | [] -> ()
  | [var, pred] -> Format.fprintf ppf "%s /*%a*/" var print_type_pred pred
  | (var, pred) :: lst -> Format.fprintf ppf "%s /*%a*/ * %a" var print_type_pred pred print_type_spec lst

let rec print_type_seq ppf = function 
  | [] -> ()
  | [(tag, spec)] -> (match spec with
    | Nothing pred -> Format.fprintf ppf "%s /*%a*/" tag print_type_pred pred
    | Just lst' -> Format.fprintf ppf "%s of %a" tag print_type_spec lst')
  | (tag, spec) :: l -> match spec with
    | Nothing pred -> Format.fprintf ppf "%s /*%a*/ | %a" tag print_type_pred pred print_type_seq l
    | Just lst' -> Format.fprintf ppf "%s of %a | %a" tag print_type_spec lst' print_type_seq l

let print_dt ppf (n, pred, lst) = Format.fprintf ppf "%s /*%a*/ = %a" n print_type_pred pred print_type_seq lst

let rec print_int_list = function
  | [] -> ()
  | [it] -> Format.printf "%d\n" it
  | hd :: tl -> Format.printf "%d, " hd; print_int_list tl