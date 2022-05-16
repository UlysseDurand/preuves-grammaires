
type 'a reglecf = int * ('a list)

type 'e cfg = {
  terminaux : ('e caractere) array ;
  reglescf : ('e caractere) reglecf array
}

type 'a arbre = 'a * 'a foret and 'a foret = F of 'a arbre list

(*remplace_alge x i m renvoit l'ensemble des {x où une occurences de (Nt i) dans x est remplacée par m}*)
let rec remplace_alge x i m =
	match x with
		|[] -> []
		|t::q -> 
			let lasuite = (remplace_alge q i m) in
			let lautresuite = List.map (fun lemot -> t::lemot) lasuite in
			match t with
				|Nt k -> if k=i then (m@q)::lautresuite else lautresuite
				|_ -> lautresuite

let lisracines foret = match foret with
|F l -> List.map fst l

let succalgeb lesregles x =
  let res =
  List.flatten
  (
    Array.to_list (
      Array.map 
      (fun (i,m) ->
        remplace_alge x i m
      )
      lesregles
    )
  )
  in
  ajouteplein res []

let chercherderivationnaifalge x m lesregles =
  parcoursmagique
  (succ ppregles)
  (fun x -> false)
  (fun x -> x = m)
  []
  [[x]]



  (*Un vérificateur de preuve d'appartenance d'un mot à une grammaire algébrique*)
let rec test_foret_deriv foret mot regles =
  let n = List.length mot in
  if n = 0 then foret = F [] else
  match foret with
      |(F sousarbres) ->
          if (List.length sousarbres) <> n then false else
          List.for_all2
          (fun arb lettre ->
              test_arbre_deriv arb regles
          )
          sousarbres
          mot
and test_arbre_deriv arb regles =
  let (lettre,sousforet) = arb in
  match lettre with
      |T x -> arb = (T x,F [])
      |Nt i ->
          let nouveaumot = lisracines sousforet in
          (List.mem (i,nouveaumot) regles)&&
          (test_foret_deriv sousforet nouveaumot regles)

(* Un exemple de grammaire algebrique *)
let exag = {
  terminaux = [|T '0';T '+';T '*';T '1'; T '"'|];
  reglescf = [|
    (0,[Nt 0 ; Nt 0 ; T '+']);
    (0,[Nt 0 ; Nt 0 ; T '*']);
    (0,[T '"' ; Nt 1 ; T '"']);
    (1,[Nt 2]);
    (1,[Nt 1 ; Nt 2]);
    (2,[T '0']);
    (2,[T '1']);
    |]
}

(* Un exemple de grammaire algebrique *)
let exfg2 = {
  terminaux = [|T '0';T '+';T '*';T '1'; T '"'|];
  nonterminaux = [|Nt 0; Nt 1; Nt 2|];
  axiome = Nt 0;
  reglesf = [|
    ([Nt 0],[Nt 0 ; Nt 0 ; T '+']);
    ([Nt 0],[Nt 0 ; Nt 0 ; T '*']);
    ([Nt 0],[T '"' ; Nt 1 ; T '"']);
    ([Nt 1],[Nt 2]);
    ([Nt 1],[Nt 1 ; Nt 2]);
    ([Nt 2],[T '0']);
    ([Nt 2],[T '1']);
    |]
}
