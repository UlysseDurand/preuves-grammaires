open Typesetutiles

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