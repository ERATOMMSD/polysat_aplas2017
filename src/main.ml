open Format

let run commands =
  List.iter (function
      | Command.Simplify f ->
          printf "%a@\n" Formula.pp f
      | Command.Interpolant (f1, f2, template, degree) ->
          Constraint.ip f1 f2 template degree |> Matlab.print_code f1 f2
    ) commands

let () =
  let load filename =
    run (Command.load filename)
  in
  Arg.parse [] load "polysat"
