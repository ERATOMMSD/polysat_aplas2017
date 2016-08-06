open Format

let print_code psds zeros ip cert =
  let syms =
    List.map Formula.Poly.Matrix.to_list_list psds
    |> List.concat |> List.concat
  in
  printf "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") Formula.Poly.pp)
    syms;
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

  printf "optimize(F);@\n";
  print_newline ();

  let vars = Formula.Poly.vars ip |> Formula.Poly.VarSet.elements in
  printf "@[<h>sdpvar %a;@]@\n"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
    vars;
  print_newline ();

  printf "@[<v>%a@]@\n"
    (pp_print_list
       (fun fmt a -> fprintf fmt "@[<h>%a = value(%a);@]" Formula.Poly.pp a Formula.Poly.pp a))
    syms;
  print_newline ();

  printf "@[<h>ip = %a;@]@\n" Formula.Poly.pp ip;
  printf "@[<h>cert = %a;@]@\n" Formula.Poly.pp cert;
  printf "sdisplay(ip);\n"
