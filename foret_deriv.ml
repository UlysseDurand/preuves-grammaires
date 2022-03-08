type 'a arbre = 'a * 'a foret and 'a foret = F of 'a arbre list

let lisracines foret = match foret with
    |F l -> List.map fst l

let rec test_foret_deriv foret mot regles =
    let n = List.length mot in
    if n = 0 then foret = F [] else
    match f with
        |(F sousarbres) where (List.length sousarbres) = n ->
            List.for_all2
            (fun arb lettre ->
                test_arbre_deriv arb lettre regles
            )
            sousarbres
            mot
        |_ -> False
and test_arbre_deriv arb regles =
    let (racine,sousforet) = arb in
    let (lettre,numregle) = racine in
    match lettre with
        |T x -> a = (T x,F [])
        |Nt i ->
            let nouveaumot = lisracines sousforet in
            (List.mem (i,nouveaumot) regles)&&
            (test_foret_deriv sousforet nouveaumot regles)

