open Format

let rec run commands =
  match commands with
  | [] -> ()
  | Command.Simplify f :: rest ->
      printf "%a@\n" Formula.pp f;
      run rest

let () =
  let load filename =
    run (Command.load filename)
  in
  Arg.parse [] load "polysat"
