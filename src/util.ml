module List = struct
  let rec repeat x n =
    if n > 0 then x :: repeat x (n - 1) else []

  let rec count fst lst =
    if fst < lst then fst :: count (fst + 1) lst else []

  let rec choose l n =
    if n > 0 then
      match l with
      | [] -> []
      | x :: xs ->
          choose xs n @ List.map (List.cons x) (choose xs (n - 1))
    else
      [[]]

  let rec mult_choose l n =
    if n > 0 then
      match l with
      | [] -> []
      | x :: xs ->
          mult_choose xs n @ List.map (List.cons x) (mult_choose l (n - 1))
    else
      [[]]
end
