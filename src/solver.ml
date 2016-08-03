open Format

let ip_candidate sys1 sys2 vars degree =
  let psdsf1, f1 = Formula.Poly.gen_cone sys1.Formula.gezs vars degree in
  let psdsf2, f2 = Formula.Poly.gen_cone sys2.Formula.gezs vars degree in
  let psdsh1, h1 = Formula.Poly.gen_ideal sys1.Formula.eqzs vars degree in
  let psdsh2, h2 = Formula.Poly.gen_ideal sys2.Formula.eqzs vars degree in
  let g = List.fold_left (fun p g -> Formula.Poly.Op.(p * g * g))
      Formula.Poly.one (sys1.Formula.neqzs @ sys2.Formula.neqzs)
  in
  let deg' = Formula.Poly.degree g in
  let deg' = if deg' = 0 then degree else degree / deg' in
  let g = Formula.Poly.Op.(g ** deg') in
  let one = Formula.PPoly.one in
  let cert = Formula.Poly.Op.(!:one + f1 + f2 + h1 + h2 + g) in
  (* printf "================================================================================@\n"; *)
  (* printf "psdsf1:@[<v>%a@]@\n" (pp_print_list Formula.Poly.Matrix.pp) psdsf1; *)
  (* printf "f1:@[%a@]@\n" Formula.Poly.pp f1; *)
  (* printf "psdsf2:@[<v>%a@]@\n" (pp_print_list Formula.Poly.Matrix.pp) psdsf2; *)
  (* printf "f2:@[%a@]@\n" Formula.Poly.pp f2; *)
  (* printf "psdsh1:@[<v>%a@]@\n" (pp_print_list Formula.Poly.Matrix.pp) psdsh1; *)
  (* printf "h1:@[%a@]@\n" Formula.Poly.pp h1; *)
  (* printf "psdsh2:@[<v>%a@]@\n" (pp_print_list Formula.Poly.Matrix.pp) psdsh2; *)
  (* printf "h2:@[%a@]@\n" Formula.Poly.pp h2; *)
  (* printf "g:@[%a@]@\n" Formula.Poly.pp g; *)
  (* printf "cert:@[%a@]@\n" Formula.Poly.pp cert; *)
  (* printf "================================================================================@\n"; *)
  let zeros = List.map snd (Formula.Poly.to_list cert) in
  let half = Formula.PPoly.const (Num.num_of_string "1/2") in
  let ip = Formula.Poly.Op.(!:half + f1 + h1 + g) in
  (psdsf1 @ psdsf2 @ psdsh1 @ psdsh2, zeros, ip)

let ip f1 f2 degree =
  let vars1 = Formula.vars f1 in
  let vars2 = Formula.vars f2 in
  let vars = Formula.Poly.VarSet.union vars1 vars2 in
  let psds, zeros, ips =
    List.fold_left
      (fun (psds, zeros, ips) sys1 ->
         List.fold_left
           (fun (psds, zeros, ips) sys2 ->
              let psds', zeros', ip = ip_candidate sys1 sys2 vars degree in
              (psds' @ psds, zeros' @ zeros, ip :: ips))
           (psds, zeros, ips) (Formula.to_dnf f2))
      ([], [], []) (Formula.to_dnf f1)
  in
  (* let assign = *)
  (*   Formula.Poly.VarSet.fold *)
  (*     (fun x assign -> Formula.Poly.VarMap.add x Formula.PPoly.zero assign) *)
  (*     (Formula.Poly.VarSet.inter vars1 vars2) *)
  (*     Formula.Poly.VarMap.empty *)
  (* in *)
  (* let common_fact = *)
  (*   List.map (fun p -> Formula.Poly.(sub p (eval assign p))) ips *)
  (*   |> List.map Formula.Poly.to_list *)
  (*   |> List.concat *)
  (*   |> List.map snd *)
  (* in *)
  (psds, zeros (* @ common_fact *), ips)
