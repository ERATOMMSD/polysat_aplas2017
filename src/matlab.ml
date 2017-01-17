open Format
open Util
open Formula
       
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


let pp_sdp form1 form2 fmt { Constraint.psds; Constraint.zeros; Constraint.ip; Constraint.certs } =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  let vars = Formula.vars ip |> Formula.Poly.VarSet.elements in

  let (zeros_nonlin, zeros_lin) =
    List.partition
      (fun t -> List.exists (fun (m,_) -> Formula.PPoly.Monomial.degree m == 0) (Formula.PPoly.to_list t)) zeros
  in

  let l = Util.List.count 0 (List.length psds) in
  let syms_simp = List.reduce_dup syms
  in
  let pp_ip do_show  =
      fprintf fmt "  ip = '';@\n";
      fprintf fmt "  @[%a@]" pp_formula ip;
      pp_print_list (fun fmt var ->
          fprintf fmt "  ip = strrep(ip, strcat('x', int2str(depends(%a))), '%a');@\n" pp_print_string var pp_print_string var;)
                    fmt
                    (List.rev vars);
      pp_force_newline fmt ();
      if do_show then
           fprintf fmt "  fprintf('interpolant := %%s\\n', ip);@\n"
  in
  (* Start print *)
  (* print certificates *)
  fprintf fmt "%% Certificates: @\n";
  pp_print_list (fun fmt (f1,f2,g,h1,h2) ->
      fprintf fmt "@[<h>%% f1: ";
      pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                    (fun fmt (a, b) -> Format.fprintf fmt "(%a: %a)" Poly.pp a Poly.pp b)
                    fmt f1;
      fprintf fmt "@]";
      pp_force_newline fmt ();
      fprintf fmt "@[<h>%% f2: ";
      pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                    (fun fmt (a, b) -> Format.fprintf fmt "(%a: %a)" Poly.pp a Poly.pp b)
                    fmt f2;
      fprintf fmt "@]" ;     
      pp_force_newline fmt ();
      fprintf fmt "@[<h>%% g: ";
      pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                    (fun fmt (a, b) -> Format.fprintf fmt "(%a: %a)" Poly.pp a Poly.pp b)
                    fmt g;
      fprintf fmt "@]"  ;    
      pp_force_newline fmt ();
      fprintf fmt "@[<h>%% h1: ";
      pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                    (fun fmt (a, b, c) -> Format.fprintf fmt "(%a: %a: %a)" Poly.pp a Poly.pp b Poly.pp c)
                    fmt h1;
      fprintf fmt "@]" ;     
      pp_force_newline fmt ();
      fprintf fmt "@[<h>%% h2: ";
      pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                    (fun fmt (a, b, c) -> Format.fprintf fmt "(%a: %a: %a)" Poly.pp a Poly.pp b Poly.pp c)
                    fmt h2;
      fprintf fmt "@]"  ;    
      pp_force_newline fmt ();

 )
                fmt certs;
  pp_force_newline fmt ();
  (* Definition of sdpvar such as a_0,...*)
  let print_sdpvar () =
  fprintf fmt "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") Formula.Poly.pp)
    syms;
  pp_force_newline fmt ();
  in
  print_sdpvar ();
  (* Definition of sdpvar such as x,y,z,...*)
  let print_var () = 
  fprintf fmt "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
    vars;
  pp_force_newline fmt ();
  in
  print_var ();
  (* Definition of semidefinite matrix *)
  let assign_Q () =
    fprintf fmt "@[<v>%a@]@\n"
    (pp_print_list
       (fun fmt (i, m) -> fprintf fmt "@[<h>Q%d = %a;@]" i Formula.Poly.Matrix.pp m))
    (List.combine l psds);
    pp_force_newline fmt ();
  in  
  assign_Q ();
  (* Definition of constraints *)
  fprintf fmt "F = [@[%a;@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt d -> fprintf fmt "Q%d >= 0" d))
    l;
  fprintf fmt "@[<v>%a@]@];@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
       (fun fmt c -> fprintf fmt "@[<h>%a == 0@]" Formula.PPoly.pp c))
    zeros_lin;
  fprintf fmt "@[<v>%a@]@]];@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
       (fun fmt c -> fprintf fmt "@[<h>%a >= 0@]" Formula.PPoly.pp c))
    zeros_nonlin;
  pp_force_newline fmt ();
  (* Run solver *)
  fprintf fmt "ret = optimize(F);@\n";
  (* Record original solution *)
  fprintf fmt "sol = containers.Map;@\n";
  ignore (List.map (fun a -> fprintf fmt "sol('%a') = %a;@\n" Formula.Poly.pp a Formula.Poly.pp a) syms_simp);
  pp_force_newline fmt ();


  (* Assign solution *)
  fprintf fmt "  @[<v>%a@]@\n"
    (pp_print_list
       (fun fmt a -> fprintf fmt "@[<h>%a = value(%a);@]" Formula.Poly.pp a Formula.Poly.pp a))
    syms;
  pp_force_newline fmt ();

  fprintf fmt "if ret.problem == 0@\n"; (* if ret.problem *)
  (* show raw interpolant *)
  pp_ip true;

  fprintf fmt "%% Test: variables are %a @\n"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) (syms_simp);

  (* approximate sdp variables by diophantine *)
  fprintf fmt "orig = [%a];@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) (syms_simp);
  fprintf fmt "dio = approximate2(sym(round(orig, round_digits)), depth);@\n";
  for i = 0 to (List.length syms_simp - 1) do
    fprintf fmt "%a = dio(%i);@\n" Formula.Poly.pp (List.nth syms_simp i) (i + 1); 
  done;
                 

  (* check validity *)
  fprintf fmt "valid = true;@\n";
  fprintf fmt "fprintf('Checking semidefiniteness...\\n');@\n";
  assign_Q ();
  fprintf fmt "@[<h>valid = valid & %a;@]@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") (fun fmt i -> fprintf fmt "check_psd_sym(Q%i)" i)) (List.count 0 (List.length psds));
  
  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking strictcone condition...\\n');@\n";
  pp_print_list (fun fmt -> fprintf fmt "@[<h>valid = valid & isAlways(1 + %a > 0);@]@\n" Formula.PPoly.pp) fmt zeros_nonlin;
  
  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking equality...\\n');@\n";
  ignore (List.map (fun p ->
              fprintf fmt "@[<h>valid = valid & isAlways(@[<h>%a@] == 0);@]@\n"  Formula.PPoly.pp p;
    ) zeros_lin);
  
  pp_force_newline fmt ();  

  pp_ip true;
  fprintf fmt "if valid@\n"; (* if - 1st check *)
  fprintf fmt "  fprintf('The interpolant is valid.\\n');@\n";
  fprintf fmt "else@\n"; (* if - 1st check *)  
  fprintf fmt "  fprintf('The interpolant is invalid.\\n');@\n";  
  fprintf fmt "end@\n";  (* end - 1st check *)

  fprintf fmt "elseif ret.problem == 1@\n"; (* elseif of ret.problem*)
  fprintf fmt "  disp('Infeasible');@\n";
  fprintf fmt "else@\n"; (* else ret.problem *)
  fprintf fmt "  disp(yalmiperror(ret.problem))@\n";
  fprintf fmt "end@\n" (* end ret.problem *)

let pp_header fmt () =
  fprintf fmt "depth = 1;@\n";
  fprintf fmt "reciprocal_epsilon = 100000;@\n";
  fprintf fmt "round_digits = 5;@\n"                    

  

let print_code f1 f2 sdps =
  printf "  @[<v>%a@]" pp_header ();   
  printf "  @[<v>%a@]" (pp_print_list (pp_sdp f1 f2)) sdps;
  printf "  return;@\n"
