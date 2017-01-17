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

  (* show raw interpolant *)
  fprintf fmt "if ret.problem == 0@\n"; (* if ret.problem *)
    fprintf fmt "  ip = '';@\n";
  fprintf fmt "  @[%a@]" pp_formula ip;
  pp_print_list (fun fmt var ->
      fprintf fmt "  ip = strrep(ip, strcat('x', int2str(depends(%a))), '%a');@\n" pp_print_string var pp_print_string var;)
                fmt
                (List.rev vars);
  pp_force_newline fmt ();
  fprintf fmt "  fprintf('interpolant := %%s\\n', ip);@\n";

  (* end show raw interpolant *)

  fprintf fmt "%% Test: variables are %a @\n"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) (syms_simp);

  (* Making linear constraints from SDP constraints*)
  fprintf fmt "A = zeros(%i, %i);@\n" (List.length zeros_lin) (List.length syms_simp);
  let sym_to_mon = (fun elt ->
      let (h,_) = List.hd (Formula.PPoly.to_list (Formula.Poly.to_const elt))
      in h)
  in
  let syms_simp_m = List.map sym_to_mon syms_simp
  in
  let ip_coefs = List.map (fun (_,c) -> c) (List.concat (List.map Poly.to_list (Formula.polys ip)))
  in
  fprintf fmt "  %% Iptest, iplen: %i@\n" (List.length ip_coefs);
  for i = 0 to (List.length zeros_lin - 1) do
    (* linear constraints *)
    let lc = Formula.PPoly.to_list (List.nth zeros_lin i) in
    pp_print_list
      (fun fmt (t, c) ->
        fprintf fmt "@[<h>A(%i, %i) = %s;@]"
                (i+1) (List.find_index ((==) t) syms_simp_m + 1) (Num.string_of_num c)) fmt lc;
    pp_force_newline fmt ();
  done;
  fprintf fmt "@[<h>A=sym(A)@];@\n";
  fprintf fmt "@[original = [%a]@];\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ")  Formula.Poly.pp) syms_simp ;
  fprintf fmt "@[<h>ips = sym(value([%a]));@]@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") PPoly.pp) ip_coefs;
  fprintf fmt "ips = approximate2(cut_epsilon(ips, reciprocal_epsilon), depth);@\n";
  print_sdpvar ();
  print_var();
  assign_Q();
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
       (fun fmt i -> fprintf fmt "@[<h>%a == double(ips(%i))@]" PPoly.pp (List.nth ip_coefs i) (i + 1)))
    (List.count 0 (List.length ip_coefs));  
  pp_force_newline fmt ();
  (* Run solver *)
  fprintf fmt "ret = optimize(F);@\n";
  fprintf fmt "if ret.problem == 0@\n"; (* if - existence of simple ip *)
  (* 1st check by linear algebra *)
  fprintf fmt "  A2 = zeros(%i, %i);@\n"  (List.length ip_coefs) (List.length syms_simp);
  fprintf fmt "  B = zeros(%i, 1);@\n" (List.length ip_coefs);  
  for i = 0 to (List.length ip_coefs - 1) do
    List.iter (fun (m, c) ->
        let ind = (List.find_index ((==) m) syms_simp_m + 1)
        in
        fprintf fmt "  A2(%i, %i) = %s;@\n" (i+1) ind (Num.string_of_num c)
      ) (PPoly.to_list (List.nth ip_coefs i));
    fprintf fmt "  B(%i, 1) = ips(%i);@\n" (i+1) (i+1);
  done;
  fprintf fmt "  A = vertcat(A, A2);@\n";
  fprintf fmt "  B = vertcat(zeros(%i, 1), B);@\n" (List.length zeros_lin);

  fprintf fmt "  [BB,UU,UUinv] = mygauss_mathematica(A);@\n";
  fprintf fmt "  bias = linsolve_mathematica(A, B);@\n";
  fprintf fmt "@['Linsolve end'@]@\n";    
  fprintf fmt "  r = double(sum(sum(BB*UU)));@\n";  
  fprintf fmt "@[original = value([%a])@];\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ")  Formula.Poly.pp) syms_simp ;
  fprintf fmt "@[original2 = original - double(bias)@];@\n";
  fprintf fmt "@[fitted = UUinv*original2@];@\n";
  fprintf fmt "@[ess = fitted(r+1:length(fitted), 1)@];@\n";
  fprintf fmt "@[ess = sym(ess)@];@\n";
  fprintf fmt "@[fitted = vertcat(zeros(double(r), 1), ess);@]@\n";
  fprintf fmt "@[app = UU*fitted + bias;@]@\n";
  for i = 0 to (List.length(syms_simp) - 1) do
    fprintf fmt "%a = app(%i, 1);@\n" Formula.Poly.pp (List.nth syms_simp i) (i + 1)
  done;

  pp_force_newline fmt ();

  fprintf fmt "valid = true;@\n";
  fprintf fmt "fprintf('Checking semidefiniteness...\\n');@\n";
  assign_Q ();
  fprintf fmt "@[<h>valid = valid & %a;@]@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") (fun fmt i -> fprintf fmt "check_psd_sym(Q%i)" i)) (List.count 0 (List.length psds));
  (* fprintf fmt "if valid == false@\n"; *)
  (* fprintf fmt "  pause;@\n";   *)
  (* fprintf fmt "end@\n"; *)
  
  
  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking strictcone condition...\\n');@\n";
  (* pp_print_list (fun fmt -> fprintf fmt "@[<h>['1 + %a = ' ]@]@\n" Formula.PPoly.pp) fmt zeros_nonlin; *)

  (* fprintf fmt "@[<h>['1 + %a = ' sdisplay(1 + %a)]@]@\n" Formula.PPoly.pp (List.hd zeros) Formula.PPoly.pp (List.hd zeros); *)
  pp_print_list (fun fmt -> fprintf fmt "@[<h>valid = valid & isAlways(1 + %a > 0);@]@\n" Formula.PPoly.pp) fmt zeros_nonlin;  
  (* fprintf fmt "if valid == false@\n"; *)
  (* fprintf fmt "  pause;@\n";   *)
  (* fprintf fmt "end@\n"; *)

  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking equality...\\n');@\n";
  ignore (List.map (fun p ->
              (* fprintf fmt "@[<h>['%a = ' ]@]@\n" Formula.PPoly.pp p ; *)
              fprintf fmt "@[<h>valid = valid & isAlways(@[<h>%a@] == 0);@]@\n"  Formula.PPoly.pp p;
              (* fprintf fmt "if valid == false@\n"; *)
              (* fprintf fmt "  pause;@\n"; *)
              (* fprintf fmt "end@\n"                                             *)
    ) zeros_lin);
  

  pp_force_newline fmt ();  

  fprintf fmt "if valid@\n"; (* if - 1st check *)
  pp_ip true;
  fprintf fmt "  fprintf('The validity was checked by linear algebra.\\n');@\n";
  fprintf fmt "else@\n";  (* else - 1st check *)
  fprintf fmt "  fprintf('The 1st validity checking failed.\\n');@\n";
  pp_ip false;
  fprintf fmt "ip_cand = ip;@\n";
  fprintf fmt "ip = '';@\n";
  fprintf fmt "%a@\n" pp_formula form1;
  fprintf fmt "form1 = ip;@\n";
  fprintf fmt "ip = '';@\n";  
  fprintf fmt "%a@\n" pp_formula form2;
  fprintf fmt "form2 = ip;@\n";
  let v = Formula.Poly.VarSet.elements (Formula.Poly.VarSet.union (Formula.vars form1) (Formula.vars form2))
  in
  fprintf fmt "vars = '%a';@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt s -> fprintf fmt "%s" s)) v;
  fprintf fmt "b = check_ip_mathematica(ip_cand, form1, form2, vars);@\n";
  fprintf fmt "if b@\n";
  pp_ip true;
  fprintf fmt "  fprintf('The 2nd validity checking passed.\\n');@\n";  
  fprintf fmt "else\n";
  fprintf fmt "  fprintf('The 2nd validity checking failed.\\n');@\n";    
  fprintf fmt "end@\n";  
  
  fprintf fmt "end@\n";  (* end - 1st check *)
  fprintf fmt "else@\n";  (* else - existence of simple ip *)
  fprintf fmt "  fprintf('Could not find a simple interpolant for the given depth.\\n');@\n";
  fprintf fmt "end@\n";(* end - existence of simple ip *)

  fprintf fmt "elseif ret.problem == 1@\n"; (* elseif of ret.problem*)
  fprintf fmt "  disp('Infeasible');@\n";
  fprintf fmt "else@\n"; (* else ret.problem *)
  fprintf fmt "  disp(yalmiperror(ret.problem))@\n";
  fprintf fmt "end@\n" (* end ret.problem *)

let pp_header fmt () =
  fprintf fmt "depth = 1;@\n";
  fprintf fmt "reciprocal_epsilon = 100000;@\n"          

  

let print_code f1 f2 sdps =
  printf "  @[<v>%a@]" pp_header ();   
  printf "  @[<v>%a@]" (pp_print_list (pp_sdp f1 f2)) sdps;
  printf "  return;@\n"
