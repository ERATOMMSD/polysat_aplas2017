open Format

let () =
  let open Formula.Poly in
  let psd, sos = sos (VarSet.of_list ["x"]) 4 in
  printf "psd:@[%a@]@\n" Matrix.pp psd;
  printf "sos:@[%a@]@\n" pp sos

let run commands =
  List.iter (function
      | Command.Simplify f ->
          printf "%a@\n" Formula.pp f
      | Command.Interpolant (f1, f2, deg) ->
          let psds, zeros, ips = Solver.ip f1 f2 deg in
          printf "psds:@[<v>%a@]@\n" (pp_print_list Formula.Poly.Matrix.pp) psds;
          printf "zeros:@[<v>%a@]@\n" (pp_print_list Formula.PPoly.pp) zeros;
          printf "ips:@[<v>%a@]@\n" (pp_print_list Formula.Poly.pp) ips
    ) commands

let () =
  let load filename =
    run (Command.load filename)
  in
  Arg.parse [] load "polysat"
