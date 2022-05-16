(*
Un parcours en largeur qui 
  -elimine des chemins passant par un sommet invalide 
  -s'arrete des qu'il parcourt un sommet valide
PS : 
  -dans avoir il y a des chemins 
  -retourne un chemin.
*)
let rec parcoursmagique delta elimine termine dejavu avoir =
  let navoir = ref [] in
  let ndejavu = ref dejavu in
  if avoir = [] then None else (
  let res = 
    List.find_opt
    (function
      |[] -> failwith "mauvais chemin"
      |s::q ->
        ndejavu := ajoute s (!ndejavu);
        if (termine s) then true else
        if (elimine s || List.mem s dejavu) then false else
        (
        navoir := 
          ajouteplein
          (
            List.map
            (fun v -> v::s::q)
            (delta s)
          )
          (!navoir);
        false)
    )
    avoir
    in
  match res with
    |None -> parcoursmagique delta elimine termine (!ndejavu) (!navoir)
    |Some x -> Some x
  )

(* Retourne les mots vers lesquels x peut dériver une fois *)
let succ ppregles x =
  let res =
  List.flatten
  (
    Array.to_list (
      Array.map
      (fun (a,b,pp) ->
        List.map 
        (fun i ->
          remplace x i (Array.length a) b
        )
        (kmp x a pp)
      )
      ppregles
    )
  )
  in
  ajouteplein res []

(* Retourne les mots vers lesquels x peut dériver une fois *)
let succbis ppregles (x,_,_) =
  let res =
  List.flatten
  (
    Array.to_list (
      Array.mapi
      (fun numregle (a,b,pp) ->
        List.map 
        (fun i ->
          (remplace x i (Array.length a) b,numregle,i)
        )
        (kmp x a pp)
      )
      ppregles
    )
  )
  in
  ajouteplein res []

(* Cherche une dérivation de x vers m *)
let chercherderivationnaif x m ppregles = 
  parcoursmagique
  (succ ppregles)
  (fun x -> false)
  (fun x -> x = m)
  []
  [[x]]

let lememeavecderivations x m ppregles =
  parcoursmagique
  (succbis ppregles)
  (fun (y,a,b) -> false)
  (fun (y,a,b) -> y = m)
  []
  [[x,0,0]]