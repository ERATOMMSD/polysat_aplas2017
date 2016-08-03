open Util.Printf

let print_code psds zeros ip =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  printf "sdpvar %a;\n" (print_list Formula.Poly.print) syms;
  print_newline ();

  let l = Util.List.count 0 (List.length psds) in
  printf "%a\n"
    (print_list ~sep:"\n"
       (fun out (i, m) -> fprintf out "Q%d = %a;" i Formula.Poly.Matrix.print m))
    (List.combine l psds);
  print_newline ();

  printf "F = [%a;\n"
    (print_list (fun out i -> fprintf out "Q%d >= 0" i)) l;
  printf "%a];\n"
    (print_list ~sep:"\n"
       (fun out c -> fprintf out "    %a == 0;" Formula.PPoly.print c)) zeros;
  print_newline ();

  printf "optimize(F);\n";
  print_newline ();

  let vars = Formula.Poly.vars ip |> Formula.Poly.VarSet.elements in
  printf "sdpvar %a;\n"
    (print_list (fun out s -> fprintf out "%s" s)) vars;
  print_newline ();

  printf "%a\n"
    (print_list ~sep:"\n"
       (fun out a ->
          fprintf out "%a = value(%a);" Formula.Poly.print a Formula.Poly.print a))
    syms;
  print_newline ();

  printf "ip = %a;\n" Formula.Poly.print ip;
  printf "sdisplay(ip);\n"
