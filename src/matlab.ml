open Format
open Util
       
let pp_and fmt () =
  fprintf fmt "ip = [ip ' && '];@\n"

let pp_or fmt () =
  fprintf fmt "ip = [ip ' || '];@\n"

let pp_ineqs eq fmt ps =
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:pp_and
       (fun fmt p ->
          fprintf fmt "@[<h>p = %a;@]@\n" Formula.Poly.pp p;
          fprintf fmt "s = sdisplay(p);@\n";
          fprintf fmt "ip = [ip s{1,1} ' %s 0'];@\n" eq))
    ps


let pp_conj fmt { Formula.eqzs; Formula.gtzs; Formula.gezs } =
  let pp_eqzs () = pp_ineqs "=" fmt eqzs in
  let pp_gtzs () = pp_ineqs ">" fmt gtzs in
  let pp_gezs () = pp_ineqs ">=" fmt gezs in
  match List.(length eqzs, length gtzs, length gezs) with
  | 0, 0, 0 ->
      fprintf fmt "ip = [ip 'True'];@\n"
  | _, 0, 0 ->
      pp_eqzs ()
  | 0, _, 0 ->
      pp_gtzs ()
  | 0, 0, _ ->
      pp_gezs ()
  | _, _, 0 ->
      pp_eqzs ();
      pp_and fmt ();
      pp_gtzs ()
  | 0, _, _ ->
      pp_gtzs ();
      pp_and fmt ();
      pp_gezs ()
  | _, 0, _ ->
      pp_eqzs ();
      pp_and fmt ();
      pp_gezs ()
  | _, _, _ ->
      pp_eqzs ();
      pp_and fmt ();
      pp_gtzs ();
      pp_and fmt ();
      pp_gezs ()


let pp_formula fmt f =
  let dnf = Formula.to_dnf f in
  if List.length dnf = 0 then
    fprintf fmt "ip = 'False';"
  else
    pp_print_list ~pp_sep:pp_or pp_conj fmt dnf


let pp_sdp fmt { Constraint.psds; Constraint.zeros; Constraint.ip } =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  fprintf fmt "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") Formula.Poly.pp)
    syms;
  pp_force_newline fmt ();

  let vars = Formula.vars ip |> Formula.Poly.VarSet.elements in
  fprintf fmt "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
    vars;
  pp_force_newline fmt ();

  let l = Util.List.count 0 (List.length psds) in
  fprintf fmt "@[<v>%a@]@\n"
    (pp_print_list
      (fun fmt (i, m) -> fprintf fmt "@[<h>Q%d = %a;@]" i Formula.Poly.Matrix.pp m))
    (List.combine l psds);
  pp_force_newline fmt ();

  fprintf fmt "F = [@[%a;@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt d -> fprintf fmt "Q%d >= 0" d))
    l;
  fprintf fmt "@[<v>%a@]@]];@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
       (fun fmt c -> fprintf fmt "@[<h>%a == 0@]" Formula.PPoly.pp c))
    zeros;
  pp_force_newline fmt ();

  (* fprintf fmt "@[<h>ip = %a;@]@\n" Formula.Poly.pp ip; *)
  (* fprintf fmt "obj = norm(coefficients(ip, [%a]), 1);@\n" *)
  (*   (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string) *)
  (*   vars; *)
  (* pp_force_newline fmt (); *)

  fprintf fmt "ret = optimize(F, 1);@\n";
  pp_force_newline fmt ();

  fprintf fmt "if ret.problem == 0@\n";
  fprintf fmt "  @[<v>%a@]@\n"
    (pp_print_list
       (fun fmt a -> fprintf fmt "@[<h>%a = value(%a);@]" Formula.Poly.pp a Formula.Poly.pp a))
    syms;
  pp_force_newline fmt ();

  let syms_simp = List.reduce_dup syms
  in
  
  fprintf fmt "%% Test: variables are %a @\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) (syms_simp);

  (* fprintf fmt "[%a] = approximate([%a], 0.001)@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) syms_simp (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) syms_simp;  *)
  fprintf fmt "A = approximate([%a], 0.01)@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) syms_simp ;
  for i = 0 to (List.length(syms_simp) - 1) do
    fprintf fmt "%a = A(%i);@\n" Formula.Poly.pp (List.nth syms_simp i) (i + 1)
  done;


  pp_force_newline fmt ();

  fprintf fmt "  ip = '';@\n";
  fprintf fmt "  @[%a@]" pp_formula ip;
  fprintf fmt "  fprintf('interpolant := %%s\\n', ip);@\n";
  fprintf fmt "  return;@\n";

  fprintf fmt "elseif ret.problem == 1@\n";
  fprintf fmt "  disp('Infeasible');@\n";
  fprintf fmt "else@\n";
  fprintf fmt "  disp(yalmiperror(ret.problem))@\n";
  fprintf fmt "end@\n"



let print_code sdps =
  printf "  @[<v>%a@]" (pp_print_list pp_sdp) sdps;
  printf "  return;@\n"
