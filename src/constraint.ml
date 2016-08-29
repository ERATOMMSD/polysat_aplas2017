(** Constraint generator for an interpolant generation problem *)

open Util

(** [ip_candidate sys1 sys2 vars degree] generates an interpolant candidate
    between semi-algebraic system (SAS) [sys1] and [sys2].

    The return value is the quadruple consisting of:
    - positive semi definite parameter variables matrix constraints,
    - parameter variables linear equality constraints,
    - an interpolant candidate, and
    - a certificate.

    In order to have an actual interpolant, solve the constraints and then
    assign values into parameter variables in the interpolant candidate.

    This generation is based on the paper \[Liyun Dai, et al. CAV 2013\].

    @param sys1 is the SAS implying the interpolant.

    @param sys2 is the SAS contradicting the interpolant.

    @param vars is the variables used in a problem space.  Note that the
    variables could contain more variables than in [sys1] and [sys2].

    @param degree is the polynomial degree used when generating cones and ideals
    in the theorem.
*)
let ip_candidate sys1 sys2 vars degree =
  let psdsf1, f1 = Formula.Poly.gen_cone (sys1.Formula.gezs @ sys1.Formula.gtzs) vars degree in
  let psdsf2, f2 = Formula.Poly.gen_cone (sys2.Formula.gezs @ sys2.Formula.gtzs) vars degree in
  let psdsg, g, zero = Formula.Poly.gen_strict_cone sys1.Formula.gtzs degree in
  let psdsh1, h1 = Formula.Poly.gen_ideal sys1.Formula.eqzs vars degree in
  let psdsh2, h2 = Formula.Poly.gen_ideal sys2.Formula.eqzs vars degree 
  in
  let cert = Formula.Poly.Op.(f1 + f2 + h1 + h2 + g) in
  let zeros = List.map snd (Formula.Poly.to_list cert) in
  let ip = Formula.Poly.Op.(f1 + h1 + g) in
  (psdsf1 @ psdsf2 @ psdsg @ psdsh1 @ psdsh2, zero :: zeros, ip, cert)

(** [ip f1 f2 template degree] generates an interpolant candidate with the same
    shape of the given interpolant [template] between two formula [f1] and [f2].
    For details see {!ip_candidate}. *)
let ip f1 f2 template degree =
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
  (* synchronize candidate ips *)
  let rev_ips = List.rev ips in
  let sync_coeffs =
    List.map2
      (fun ip1 ip2 ->
         Formula.Poly.to_list (Formula.Poly.Op.(ip1 - ip2)) |> List.map snd)
      (List.hd ips :: ips)
      (List.hd rev_ips :: rev_ips |> List.rev)
    |> List.concat
  in
  let ip = List.hd ips in
  (* synchronize template if exists *)
  let unuse_coeffs = match template with
    | None -> []
    | Some p ->
        let monos_in_templ = Formula.Poly.to_list p |> List.map fst in
        Formula.Poly.to_list ip
        |> List.map
          (fun (m, c) ->
             if List.exists (Formula.Poly.Monomial.equal m) monos_in_templ then
               None
             else
               Some c)
        |> List.reduce_options
  in
  (psds, zeros @ loose_coeffs @ sync_coeffs @ unuse_coeffs, ip, certs)
