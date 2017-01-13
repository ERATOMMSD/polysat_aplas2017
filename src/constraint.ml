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
  let psdsg1, g1, zero1 = Formula.Poly.gen_strict_cone sys1.Formula.gtzs degree in
    let psdsg2, g2, zero2 = Formula.Poly.gen_strict_cone sys2.Formula.gtzs degree in
  let psdsh1, h1 = Formula.Poly.gen_ideal sys1.Formula.eqzs vars degree in
  let psdsh2, h2 = Formula.Poly.gen_ideal sys2.Formula.eqzs vars degree in
  let cert1 = Formula.Poly.Op.(f1 + f2 + h1 + h2 + g1) in
  let zeros1 = List.map snd (Formula.Poly.to_list cert1) in
  let ip1 = Formula.Poly.Op.(f1 + h1 + g1) in
  let cert2 = Formula.Poly.Op.(f1 + f2 + h1 + h2 + g2) in
  let zeros2 = List.map snd (Formula.Poly.to_list cert2) in
  let ip2 = Formula.Poly.Op.(f2 + h2 + g2) in
  [(psdsf1 @ psdsf2 @ psdsg1 @ psdsh1 @ psdsh2, zero1 :: zeros1, ip1, [cert1], true); (psdsf1 @ psdsf2 @ psdsg2 @ psdsh1 @ psdsh2, zero2 :: zeros2, ip2, [cert2], false)]

type sdp = {
  psds : Formula.Poly.Matrix.t list;
  zeros : Formula.PPoly.t list;
  ip : Formula.t;
  certs: Formula.Poly.t list;
}

(** [ip f1 f2 template degree] generates an interpolant candidate with the same
    shape of the given interpolant [template] between two formula [f1] and [f2].
    For details see {!ip_candidate}. *)
let ip f1 f2 template degree =
  let vars1 = Formula.vars f1 in
  let vars2 = Formula.vars f2 in
  let vars = Formula.Poly.VarSet.union vars1 vars2 in
  let rec choose_cases m =
    match m with
    | [] -> [[]]
    | hd :: tl -> List.concat
                    [List.map (fun l -> (List.nth hd 0)::l) (choose_cases tl);
                     List.map (fun l -> (List.nth hd 1)::l) (choose_cases tl)]
  in
  let rec tupling_without_concat l1 l2 =
    List.map (fun a1 -> List.map (fun a2 -> (a1, a2)) l2) l1
  in
  let rec list_tupling l =
    match l with
    | [] -> [[]]
    | hd :: tl -> List.concat (List.map (fun hdhd -> List.map (fun tail -> (hdhd :: tail)) (list_tupling tl)) hd)
  in
  let sdps =
    tupling_without_concat (Formula.to_dnf f1) (Formula.to_dnf f2)
    |> List.map (List.map (fun (sys1, sys2) -> ip_candidate sys1 sys2 vars degree))
    |> List.map choose_cases
    |> list_tupling
  in
  (* interpolant should have only shared variables between f1 f2 *)
  let common_vars = Formula.Poly.VarSet.inter vars1 vars2 in
  let loose_coeffs ip =
    Formula.Poly.to_list ip
    |> List.map
         (fun (m, c) ->
           if Formula.Poly.VarSet.subset (Formula.Poly.Monomial.vars m) common_vars then
             None
           else
             Some c)
    |> List.reduce_options
  in
  let return_sdp (psds, zeros, ip, certs, strict) =
    {psds = psds; zeros = (zeros @ (loose_coeffs ip)); ip = if strict then Formula.(gt ip Poly.zero) else Formula.(le ip Poly.zero); certs = certs }
  in
  let rec return_sdpl sdplist =
    match sdplist with
    | [] -> {psds = []; zeros = []; ip = Formula.tru; certs = []}
    | hd :: tl -> {psds = (return_sdp hd).psds @ (return_sdpl tl).psds;
                   zeros = (return_sdp hd).zeros @ (return_sdpl tl).zeros;
                   ip = Formula.conjunction (return_sdp hd).ip (return_sdpl tl).ip;
                  certs = (return_sdp hd).certs @ (return_sdpl tl).certs}
  in
  let rec return_sdpll sdplistlist =
    match sdplistlist with
    | [] -> {psds = []; zeros = []; ip = Formula.fls; certs = []}
    | hd :: tl -> {psds = (return_sdpl hd).psds @ (return_sdpll tl).psds;
                   zeros = (return_sdpl hd).zeros @ (return_sdpll tl).zeros;
                   ip = Formula.disjunction (return_sdpl hd).ip (return_sdpll tl).ip;
                  certs = (return_sdpl hd).certs @ (return_sdpll tl).certs}
  in
    List.map return_sdpll sdps
