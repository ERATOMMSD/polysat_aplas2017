open Format

let run commands =
  List.iter (function
      | Command.Simplify f ->
          printf "%a@\n" Formula.pp f
      | Command.Interpolant (f1, f2, template, degree) ->
          let psds, zeros, ip, certs = Constraint.ip f1 f2 template degree in
          Matlab.print_code psds zeros ip (List.hd certs)
    ) commands

let () =
  let load filename =
    run (Command.load filename)
  in
  Arg.parse [] load "polysat"
