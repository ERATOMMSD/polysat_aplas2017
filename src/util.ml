module Printf = struct
  include Printf

  let print_list ?(sep=" ") p out l =
    match l with
    | [] -> fprintf out ""
    | x :: xs ->
        p out x;
        List.iter (fun x -> fprintf out "%s%a" sep p x) xs
end

module List = struct
  include List

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

  let rec tupling l1 l2 =
    List.map (fun a1 -> List.map (fun a2 -> (a1, a2)) l2) l1
    |> List.concat

  let rec reduce_options = function
    | [] -> []
    | None :: xs -> reduce_options xs
    | Some x :: xs -> x :: reduce_options xs
                                          
  let rec reduce_dup l =
    List.fold_left
      (fun acc x -> if (exists ((==) x) acc)
                    then acc
                    else x::acc)
      [] l

end
