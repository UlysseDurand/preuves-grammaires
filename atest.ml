let rec n_cartesian_product = function
  | [] -> [[]]
  | x :: xs ->
    let rest = n_cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
  ;;


n_cartesian_product [[4;7] ; [1;0] ; [3;8]];;