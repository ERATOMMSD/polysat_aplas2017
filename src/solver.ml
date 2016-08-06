open Util
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
  let zeros = List.map snd (Formula.Poly.to_list cert) in
  let half = Formula.PPoly.const (Num.num_of_string "1/2") in
  let ip = Formula.Poly.Op.(!:half + f1 + h1 + g) in
  (psdsf1 @ psdsf2 @ psdsh1 @ psdsh2, zeros, ip, cert)

let ip f1 f2 degree =
  let vars1 = Formula.vars f1 in
  let vars2 = Formula.vars f2 in
  let vars = Formula.Poly.VarSet.union vars1 vars2 in
  let psds, zeros, ips, certs =
    List.tupling (Formula.to_dnf f1) (Formula.to_dnf f2)
    |> List.map (fun (sys1, sys2) -> ip_candidate sys1 sys2 vars degree)
    |> List.fold_left
      (fun (psds, zeros, ips, certs) (psds', zeros', ip, cert) ->
         (psds' @ psds, zeros' @ zeros, ip :: ips, cert :: certs))
      ([], [], [], [])
  in
  (* interpolant should have only shared variables between f1 f2 *)
  let common_vars = Formula.Poly.VarSet.inter vars1 vars2 in
  let loose_coeffs =
    List.map Formula.Poly.to_list ips
    |> List.concat
    |> List.map
      (fun (m, c) ->
         if Formula.Poly.VarSet.subset (Formula.Poly.Monomial.vars m) common_vars then
           None
         else
           Some c)
    |> List.reduce_options
  in
  (* synchronize ips *)
  let rev_ips = List.rev ips in
  let sync_coeffs =
    List.map2
      (fun ip1 ip2 ->
         Formula.Poly.to_list (Formula.Poly.Op.(ip1 - ip2)) |> List.map snd)
      (List.hd ips :: ips)
      (List.hd rev_ips :: rev_ips |> List.rev)
    |> List.concat
  in
  (psds, zeros @ loose_coeffs @ sync_coeffs, (List.hd ips), certs)
