let rec n_cartesian_product = function
  | [] -> [[]]
  | x :: xs ->
    let rest = n_cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
  ;;


n_cartesian_product [[4;7] ; [1;0] ; [3;8]];;

let parcours_spe a = 
  let rec aux foret dejavu =
    if toutfeuille foret then listoutfeuille foret else
      let laforet = if finhauteur foret dejavu then
        List.flatten (List.map (fun x -> match x with |B(_) -> [x] |N(r,l) -> l))
      else foret in
      let a,b,c = trouve laforet dejavu in
      let filsb = match b with |B(_) -> failwith "erreur" | N(r,l) -> l in
      let filsdansa = List.flatten (List.map (fun x -> match x with |B(_) -> [x] |N(r,l)->l) a ) in
      ((lisracines filsdansa)@(lisracines filsb)@c)::(aux foret (b::dejavu))
  aux [] [];;
