open Format

let run commands =
  List.iter (function
      | Command.Simplify f ->
          printf "%a@\n" Formula.pp f
      | Command.Interpolant (f1, f2, deg) ->
          let psds, zeros, ips, certs = Solver.ip f1 f2 deg in
          Matlab.print_code psds zeros (List.hd ips) (List.hd certs)
    ) commands

let () =
  let load filename =
    run (Command.load filename)
  in
  Arg.parse [] load "polysat"
