open Format

let pp_ineqs eq fmt ps =
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "disp('&&');@\n")
       (fun fmt p ->
          fprintf fmt "@[<h>p = %a;@]@\n" Formula.Poly.pp p;
          fprintf fmt "sdisplay(p);@\n";
          fprintf fmt "disp('%s 0');@\n" eq))
    ps


let pp_conj fmt { Formula.eqzs; Formula.gtzs; Formula.gezs } =
  let pp_eqzs () = pp_ineqs "=" fmt eqzs in
  let pp_gtzs () = pp_ineqs ">" fmt gtzs in
  let pp_gezs () = pp_ineqs ">=" fmt gezs in
  match List.(length eqzs, length gtzs, length gezs) with
  | 0, 0, 0 ->
      fprintf fmt "disp('True');@\n"
  | _, 0, 0 ->
      pp_eqzs ()
  | 0, _, 0 ->
      pp_gtzs ()
  | 0, 0, _ ->
      pp_gezs ()
  | _, _, 0 ->
      pp_eqzs ();
      fprintf fmt "disp('&&');@\n";
      pp_gtzs ()
  | 0, _, _ ->
      pp_gtzs ();
      fprintf fmt "disp('&&');@\n";
      pp_gezs ()
  | _, 0, _ ->
      pp_eqzs ();
      fprintf fmt "disp('&&');@\n";
      pp_gezs ()
  | _, _, _ ->
      pp_eqzs ();
      fprintf fmt "disp('&&');@\n";
      pp_gtzs ();
      fprintf fmt "disp('&&');@\n";
      pp_gezs ()


let pp_formula fmt f =
  let dnf = Formula.to_dnf f in
  if List.length dnf = 0 then
    fprintf fmt "disp('False');"
  else
    fprintf fmt "%a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "disp('||');@\n") pp_conj)
      dnf


let pp_sdp { Constraint.psds; Constraint.zeros; Constraint.ip } =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  printf "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") Formula.Poly.pp)
    syms;
  print_newline ();

  let vars = Formula.vars ip |> Formula.Poly.VarSet.elements in
  printf "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
    vars;
  print_newline ();

  let l = Util.List.count 0 (List.length psds) in
  printf "@[<v>%a@]@\n"
    (pp_print_list
      (fun fmt (i, m) -> fprintf fmt "@[<h>Q%d = %a;@]" i Formula.Poly.Matrix.pp m))
    (List.combine l psds);
  print_newline ();

  printf "F = [@[%a;@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt d -> fprintf fmt "Q%d >= 0" d))
    l;
  printf "@[<v>%a@]@]];@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
       (fun fmt c -> fprintf fmt "@[<h>%a == 0@]" Formula.PPoly.pp c))
    zeros;
  print_newline ();

  (* printf "@[<h>ip = %a;@]@\n" Formula.Poly.pp ip; *)
  (* printf "obj = norm(coefficients(ip, [%a]), 1);@\n" *)
  (*   (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string) *)
  (*   vars; *)
  (* print_newline (); *)

  printf "ret = optimize(F, 1);@\n";
  print_newline ();

  printf "if ret.problem == 0@\n";
  printf "  @[<v>%a@]@\n"
    (pp_print_list
       (fun fmt a -> fprintf fmt "@[<h>%a = value(%a);@]" Formula.Poly.pp a Formula.Poly.pp a))
    syms;
  print_newline ();

  printf "  disp('interpolant :=');@\n";
  printf "  @[%a@]" pp_formula ip;
  print_newline ();
  printf "  exit(0);@\n";

  printf "elseif ret.problem == 1@\n";
  printf "  disp('Infeasible');@\n";
  printf "else@\n";
  printf "  disp('Something else happened')@\n";
  printf "end@\n"


let print_code sdps =
  List.iter pp_sdp sdps;
  print_newline ();
  printf "exit(1);@\n"
