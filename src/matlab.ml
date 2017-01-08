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

  let syms_ip = Formula.syms_ip ip in

  let (zeros_nonlin, zeros_lin) =
    List.partition
      (fun t -> List.exists (fun (m,_) -> Formula.PPoly.Monomial.degree m == 0) (Formula.PPoly.to_list t)) zeros
  in

  let l = Util.List.count 0 (List.length psds) in
  let assign_Q = (fun () ->
    fprintf fmt "@[<v>%a@]@\n"
            (pp_print_list
               (fun fmt (i, m) -> fprintf fmt "@[<h>Q%d = %a;@]" i Formula.Poly.Matrix.pp m))
            (List.combine l psds);) in
  assign_Q ();
  pp_force_newline fmt ();

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

  let syms_simp = List.reduce_dup syms
  in
  fprintf fmt "ret = optimize(F);@\n";
  fprintf fmt "sol = containers.Map;@\n";
  ignore (List.map (fun a -> fprintf fmt "sol('%a') = %a;@\n" Formula.Poly.pp a Formula.Poly.pp a) syms_simp);
  pp_force_newline fmt ();

  fprintf fmt "if ret.problem == 0@\n";
  fprintf fmt "  @[<v>%a@]@\n"
    (pp_print_list
       (fun fmt a -> fprintf fmt "@[<h>%a = value(%a);@]" Formula.Poly.pp a Formula.Poly.pp a))
    syms;
  pp_force_newline fmt ();

  fprintf fmt "%% Test: variables are %a @\n"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")  Formula.Poly.pp) (syms_simp);

  fprintf fmt "%% Approximating coefficients in IP.@\n";
  fprintf fmt "  ips = sym(value([%a]));@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") (fun fmt -> Format.fprintf fmt "a%i")) (Formula.PPoly.VarSet.elements syms_ip);  
  fprintf fmt "ips = approximate2(cut_epsilon(ips, 100000), depth);@\n";

  (* Making linear constraints from SDP constraints*)
  fprintf fmt "A = zeros(%i, %i);@\n" (List.length zeros_lin) (List.length syms_simp);
  for i = 0 to (List.length zeros_lin - 1) do
    let sym_to_mon = (fun elt ->
        let (h,_) = List.hd (Formula.PPoly.to_list (Formula.Poly.to_const elt))
        in h)
    in
    let syms_simp_m = List.map sym_to_mon syms_simp in
    (* linear constraints *)
    let lc = Formula.PPoly.to_list (List.nth zeros_lin i) in
    pp_print_list
      (fun fmt (t, c) ->
        fprintf fmt "@[<h>A(%i, %i) = %s;@]"
                (i+1) (List.find_index ((==) t) syms_simp_m + 1) (Num.string_of_num c)) fmt lc;
    pp_force_newline fmt ();
  done;

  (* Making constant constraints 
  
  fprintf fmt "@[<h>A=sym(A)@];@\n";
  fprintf fmt "@[if skip_gauss@]\n"; (* if of skip_gauss *)
  fprintf fmt "@[load('B_num.dat', '-ascii');@]\n";
  fprintf fmt "@[load('B_den.dat', '-ascii');@]\n";
  fprintf fmt "@[B_num = sym(B_num);@]\n";
  fprintf fmt "@[B_den = sym(B_den);@]\n";
  fprintf fmt "@[B = B_num./B_den;@]\n";
  fprintf fmt "@[load('U_num.dat', '-ascii');@]\n";
  fprintf fmt "@[load('U_den.dat', '-ascii');@]\n";
  fprintf fmt "@[U_num = sym(U_num);@]\n";
  fprintf fmt "@[U_den = sym(U_den);@]\n";
  fprintf fmt "@[U = U_num./U_den;@]\n";        
  fprintf fmt "@[Uinv_num = sym(Uinv_num);@]\n";
  fprintf fmt "@[Uinv_den = sym(Uinv_den);@]\n";
  fprintf fmt "@[Uinv = Uinv_num./Uinv_den;@]\n";          
  fprintf fmt "@[else@]\n"; (* else of skip_gauss *)    
  fprintf fmt "@[if strcmp(simplify_method, '-easy_gauss')@]\n";
  fprintf fmt "@[  [B,U,Uinv] = mygaussd(A)@];@\n";
  fprintf fmt "@[elseif strcmp(simplify_method, '-smith')@]@\n";
  fprintf fmt "@[  [B,U,V,Uinv,Vinv] = mysmith(A)@];@\n";
  fprintf fmt "@[  U = sym(V);@\n";
  fprintf fmt "@[  Uinv = sym(Vinv);@\n";  
  fprintf fmt "@[else@]@\n";  
  fprintf fmt "@[  [B,U,Uinv] = mygauss(A)@];@\n";
  fprintf fmt "@[end@]@\n";
  fprintf fmt "@[[B_num,B_den] = numden(B);@]\n";
  fprintf fmt "@[B_num = double(B_num);@]\n";
  fprintf fmt "@[B_den = double(B_den);@]\n";
  fprintf fmt "@[save('B_num.dat', 'B_num', '-ascii');@]\n";
  fprintf fmt "@[save('B_den.dat', 'B_den', '-ascii');@]\n";        
  fprintf fmt "@[[U_num,U_den] = numden(U);@]\n";
  fprintf fmt "@[U_num = double(U_num);@]\n";
  fprintf fmt "@[U_den = double(U_den);@]\n";
  fprintf fmt "@[save('U_num.dat', 'U_num', '-ascii');@]\n";
  fprintf fmt "@[save('U_den.dat', 'U_den', '-ascii');@]\n";          
  fprintf fmt "@[[Uinv_num,Uinv_den] = numden(Uinv);@]\n";
  fprintf fmt "@[Uinv_num = double(Uinv_num);@]\n";
  fprintf fmt "@[Uinv_den = double(Uinv_den);@]\n";
  fprintf fmt "@[save('Uinv_num.dat', 'Uinv_num', '-ascii');@]\n";
  fprintf fmt "@[save('Uinv_den.dat', 'Uinv_den', '-ascii');@]\n";            
  fprintf fmt "@[end@]\n";  (* end of skip_gauss *)
  fprintf fmt "@['Gauss'@]@\n";  
  (* fprintf fmt "@[<h>B = sym(B)@];@\n";       *)
  (* fprintf fmt "@[<h>U = sym(U)@];@\n"; *)
  fprintf fmt "r = rank(A);@\n";
  (* fprintf fmt "r = double(sum(sum(B*U)));@\n"; *)
  fprintf fmt "@[original = [%a]@];\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ")  Formula.Poly.pp) syms_simp ;
  fprintf fmt "@[fitted = Uinv*original@];@\n";
  fprintf fmt "@[ess = fitted(r+1:length(fitted), 1)@];@\n";
  (* fprintf fmt "@[ess = sym(ess)@];@\n";   *)
  (* fprintf fmt "@[ess = ess/max(abs(ess));@];@\n";     *)
  fprintf fmt "@[if do_approximate@];@\n";
  (* fprintf fmt "  @[ess = double(ess);@];@\n"; *)
  fprintf fmt "  m = max(abs(ess));@\n";
  (* fprintf fmt "  for i=1:length(ess);@\n"; *)
  (* fprintf fmt "    if abs(ess(i)) < m/100000@\n"; *)
  (* fprintf fmt "      ess(i) = 0;@\n";   *)
  (* fprintf fmt "    end@\n";     *)
  (* fprintf fmt "  end;@\n";     *)
  fprintf fmt "  @[ess = approximate2(cut_epsilon(ess, 100000), depth);@];@\n";
  fprintf fmt "@[else@];@\n";
  fprintf fmt "  @[ess = double(ess);@];@\n";
  fprintf fmt "  @[ess = ess/max(abs(ess));@];@\n";
  fprintf fmt "  @[[ess1,ess2] = rat(ess)@];@\n";
  fprintf fmt "  @[ess = sym(ess1./ess2)@];@\n";
  fprintf fmt "@[end@];@\n";  
  
  (* fprintf fmt "@[ess = approximate(ess, 0.01);@];@\n"; *)
  (* fprintf fmt "@[ess = transpose(ess);@];@\n";   *)
  fprintf fmt "@[fitted = vertcat(zeros(r, 1), ess);@]@\n";
  fprintf fmt "@[app = U*fitted;@]@\n";
  (* fprintf fmt "@[app = app/mygcd(app);@]@\n"; *)
  (* fprintf fmt "@[app = double(app);@]@\n";     *)
  for i = 0 to (List.length(syms_simp) - 1) do
    fprintf fmt "%a = app(%i, 1);@\n" Formula.Poly.pp (List.nth syms_simp i) (i + 1)
  done;


  pp_force_newline fmt ();

  fprintf fmt "valid = true;@\n";
  fprintf fmt "fprintf('Checking semidefiniteness...\\n');@\n";
  assign_Q ();
  fprintf fmt "@[<h>valid = valid & %a;@]@\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") (fun fmt i -> fprintf fmt "check_psd_sym(Q%i)" i)) (List.count 0 (List.length psds));
  fprintf fmt "if valid == false@\n";
  fprintf fmt "  pause;@\n";  
  fprintf fmt "end@\n";
  
  
  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking strictcone condition...\\n');@\n";
  pp_print_list (fun fmt -> fprintf fmt "@[<h>['1 + %a = ' ]@]@\n" Formula.PPoly.pp) fmt zeros_nonlin;

  (* fprintf fmt "@[<h>['1 + %a = ' sdisplay(1 + %a)]@]@\n" Formula.PPoly.pp (List.hd zeros) Formula.PPoly.pp (List.hd zeros); *)
  fprintf fmt "if ignore_sc == false@\n";
  pp_print_list (fun fmt -> fprintf fmt "@[<h>valid = valid & isAlways(1 + %a > 0);@]@\n" Formula.PPoly.pp) fmt zeros_nonlin;  
  fprintf fmt "if valid == false@\n";
  fprintf fmt "  pause;@\n";  
  fprintf fmt "end@\n";
  fprintf fmt "end@\n";  

  pp_force_newline fmt ();
  fprintf fmt "fprintf('Checking equality...\\n');@\n";
  ignore (List.map (fun p ->
              (* fprintf fmt "@[<h>['%a = ' sdisplay(%a)]@]@\n" Formula.PPoly.pp p  Formula.PPoly.pp p; *)
              fprintf fmt "@[<h>['%a = ' ]@]@\n" Formula.PPoly.pp p ;
              fprintf fmt "@[<h>valid = valid & isAlways(@[<h>%a@] == 0);@]@\n"  Formula.PPoly.pp p;
              fprintf fmt "if valid == false@\n";
              fprintf fmt "  pause;@\n";
              fprintf fmt "end@\n"                                            
    ) zeros_lin);
  

  pp_force_newline fmt ();  

  (* for i = 0 to (List.length(syms_simp) - 1) do *)
  (*   fprintf fmt "%a = double(%a);@\n" Formula.Poly.pp (List.nth syms_simp i) Formula.Poly.pp (List.nth syms_simp i); *)
  (* done; *)

  
  fprintf fmt "  ip = '';@\n";
  fprintf fmt "  @[%a@]" pp_formula ip;
  fprintf fmt "  %% Iptest: %a@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") (fun fmt -> Format.fprintf fmt "a%i")) (Formula.PPoly.VarSet.elements syms_ip);
  fprintf fmt "  %% Iptest, iplen: %i@\n" (Formula.PPoly.VarSet.cardinal syms_ip);
  fprintf fmt "  %% Iptest, varlen: %i@\n" (List.length syms_simp);
  fprintf fmt "  x = [%a]@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") (fun fmt -> Format.fprintf fmt "sol('a%i')")) (Formula.PPoly.VarSet.elements syms_ip);  
  pp_print_list (fun fmt var ->
      fprintf fmt "  ip = strrep(ip, strcat('x', int2str(depends(%a))), '%a');@\n" pp_print_string var pp_print_string var;)
                fmt
                (List.rev vars);
  pp_force_newline fmt ();
  fprintf fmt "  fprintf('interpolant := %%s\\n', ip);@\n";
  fprintf fmt "if valid@\n";
  fprintf fmt "  fprintf('This interpolant is valid.\\n');@\n";
  fprintf fmt "else@\n";
  fprintf fmt "  fprintf('This interpolant is invalid.\\n');@\n";
  fprintf fmt "end@\n";
  fprintf fmt "  return;@\n";

  fprintf fmt "elseif ret.problem == 1@\n";
  fprintf fmt "  disp('Infeasible');@\n";
  fprintf fmt "else@\n";
  fprintf fmt "  disp(yalmiperror(ret.problem))@\n";
  fprintf fmt "end@\n"

let pp_header fmt () =
  fprintf fmt "tolerance = 0.01;@\n";
  fprintf fmt "do_approximate = true;@\n";
  (* fprintf fmt "easy_gauss = false;@\n"; *)
  fprintf fmt "simplify_method = '-gauss'; %% -gauss, -smith, -easy_gauss@\n";
  fprintf fmt "skip_gauss = false;@\n";
  fprintf fmt "ignore_sc = false;@\n";
  fprintf fmt "depth = 3;@\n"

let pp_sdp2 fmt { Constraint.psds; Constraint.zeros; Constraint.ip } =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  let vars = Formula.vars ip |> Formula.Poly.VarSet.elements in
  let syms_ip = Formula.syms_ip ip in
  let (zeros_nonlin, zeros_lin) =
    List.partition
      (fun t -> List.exists (fun (m,_) -> Formula.PPoly.Monomial.degree m == 0) (Formula.PPoly.to_list t)) zeros
  in
  let syms_simp = List.reduce_dup syms in

  let pp_runsdp add_constr sc_cond =
    fprintf fmt "@[<h>sdpvar %a;@]@\n"
            (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") Formula.Poly.pp)
            syms;
    pp_force_newline fmt ();
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
    fprintf fmt "@[<v>%a@]@];@\n"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
                           (fun fmt c -> fprintf fmt "@[<h>%a == 0@]" Formula.PPoly.pp c))
            zeros_lin;
    if sc_cond then
      fprintf fmt "@[<v>%a@]@];@\n"
              (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
                        (fun fmt c -> fprintf fmt "@[<h>%a >= 0@]" Formula.PPoly.pp c))
              zeros_nonlin;
    fprintf fmt "@[<v>%a@]@]];@\n"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,")
                           (fun fmt (c, d) -> fprintf fmt "@[<h>a%i == double(ips(%i))@]" c d))
            add_constr;
    pp_force_newline fmt ();
    fprintf fmt "ret = optimize(F);@\n";
    pp_force_newline fmt ();
    fprintf fmt "@[original = sym(value([%a]))@];\n" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ")  Formula.Poly.pp) syms_simp ;
  in
  (* main *)
  pp_runsdp [] true;
  fprintf fmt "  ips = sym(value([%a]));@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") (fun fmt -> Format.fprintf fmt "a%i")) (Formula.PPoly.VarSet.elements syms_ip);  
  fprintf fmt "ips = approximate2(cut_epsilon(ips, 100000), depth);@\n";
  for i = 0 to ((Formula.PPoly.Monomial.VarSet.cardinal syms_ip) - 1) do
    fprintf fmt "a%i = ips(%i);@\n" (List.nth (Formula.PPoly.VarSet.elements syms_ip) i) (i + 1)
  done;
  fprintf fmt "if ret.problem == 0@\n";
  
  fprintf fmt "  ip = '';@\n";
  fprintf fmt "  @[%a@]" pp_formula ip;
  fprintf fmt "  %% Iptest: %a@\n" (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") (fun fmt -> Format.fprintf fmt "a%i")) (Formula.PPoly.VarSet.elements syms_ip);
  fprintf fmt "  %% Iptest, iplen: %i@\n" (Formula.PPoly.VarSet.cardinal syms_ip);
  fprintf fmt "  %% Iptest, varlen: %i@\n" (List.length syms_simp);
    pp_print_list (fun fmt var ->
      fprintf fmt "  ip = strrep(ip, strcat('x', int2str(depends(%a))), '%a');@\n" pp_print_string var pp_print_string var;)
                fmt
                (List.rev vars);
    fprintf fmt "  fprintf('interpolant := %%s\\n', ip);@\n";
    pp_runsdp (List.map (fun i -> ((List.nth (Formula.PPoly.VarSet.elements syms_ip) i), i+1)) (List.count 0 (Formula.PPoly.VarSet.cardinal syms_ip))) false;
    fprintf fmt "  if ret.problem == 0@\n";
    fprintf fmt "    fprintf('This interpolant is valid.\\n');@\n";        
    fprintf fmt "  else@\n";
    fprintf fmt "    fprintf('This interpolant is invalid.\\n');@\n";            
    fprintf fmt "  end@\n";    
    fprintf fmt "end@\n";  
    pp_force_newline fmt ()
  

let print_code sdps =
  printf "  @[<v>%a@]" pp_header ();   
  printf "  @[<v>%a@]" (pp_print_list pp_sdp2) sdps;
  printf "  return;@\n"
