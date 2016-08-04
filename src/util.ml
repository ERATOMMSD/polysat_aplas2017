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

  let rec tupling (ll : 'a list list) : 'a list list =
    match ll with
    | [] -> [[]]
    | l :: ls ->
        let ls = tupling ls in
        List.map (fun x -> List.map (List.cons x) ls) l |> List.concat
end
