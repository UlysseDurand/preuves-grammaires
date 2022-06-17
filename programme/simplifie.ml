(* Donne cat(m) pour des etats Q donnes par fctcat et pour la grammaire gram*)
let categorisemot gram fctcat m = Array.map fctcat (analysemot m gram)

(* Fonction categorisante binaire *)
let fct_cat_bin n = if n > 0 then 1 else 0

(* Fonction cat pour notre categorisation *)
let cat gram = categorisemot gram fct_cat_bin

(* Verifie si il existe un mot de categorie q derivable via la derivation d dans la grammaire gram *)
let estpossible q d gram =
  let (a,b) = d in
  ordre (cat gram a) q

(* Construit le graphe A *)
let grapheA gram =
  fun q ->
    let leslettres = lettres gram in
    List.concat_map 
    (fun (i,(a,b)) ->  
      let leres =
      List.filter
        (fun qp ->
          (ordre (cat gram a) q) &&
          (ordre (moins q (cat gram a)) qp) &&
          (
            List.for_all
            (fun l ->
              (
                implies ((cat gram b).(l) = 1) (qp.(l) = 1)
              )&&(
                implies (q.(l) = 0 && (cat gram b).(l) = 0) (qp.(l) = 0)
              )
            )
            (Array.to_list (Array.mapi (fun i x -> i) (Array.make (Array.length leslettres) 0)))
          )
        )
        (enarray (nuplets (Array.length leslettres)))
      (*in (i,leres)*)
      in leres
    )
    (Array.to_list (Array.mapi (fun i x -> (i,x) ) gram.reglesf) )

(* Finalement, la fonction interdit voulue *)
let interditfort gram m x =
  not (List.mem (cat gram m) (bfs (grapheA gram) [] [cat gram x] ) )

(* Recherche une preuve que l'on peut deriver x en m en utilisant la simplification de complexite *)
let recherchepreuve gram ppregles x m = 
  parcoursmagique
  (succbis ppregles)
  (fun (y,a,b) -> let res = interditfort gram m y in if res then print_int 1 ; res)
  (fun (y,a,b) -> y = m)
  []
  [[x,0,0]]
  
  
  
