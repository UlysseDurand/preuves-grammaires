(* Fonction categorisante binaire *)
let fct_cat_bin n = if n > 0 then 1 else 0

(* Verifie si il existe un mot de categorie q derivable via la derivation d dans la grammaire gram *)
let estpossible q d gram =
  let (a,b) = d in
  let cata = categorisemot gram fct_cat_bin a in 
  q = cata

(* Donne l'ensemble des indices des derivations faisables depuis l'etat q *)
let lesucc gram q =
  List.map 
  fst
  (
    List.filter
    (
      fun (i,d) -> estpossible q d gram
    )
    (
      Array.to_list
      (
        Array.mapi (fun i d -> (i,d)) gram.reglesf
      )
    )
  ) 

let vrailesucc gram q =
  

(* Donne, pour la derivation i, l'etat-couple vers lequel on arrive *)
let etatsdederiv gram i = 
  let (a,b) = gram.reglesf.(i) in
  (i,categorisemot gram fct_cat_bin b)

(* Donne, les couple de categories des derivations d'une grammaire*)
let pretraitegram gram = 
  Array.map
  (fun (a,b) -> (categorisemot gram fct_cat_bin a,categorisemot gram fct_cat_bin b)) 
  gram.reglesf

(*  *)


(* Donne une table d'association des etats accessibles *)
let pretraitebisgram gram = 
  let avoir = ref [categorisemot gram fct_cat_bin [|Nt 0|]] in
  let dejavu = ref [] in
  while (!avoir != []) do
    let t::q = !avoir in 
    if not (List.mem t (!dejavu)) then ( 
      let suivants = lesucc gram t in
      let vraisuivants = (
        List.map 
        (fun i -> categorisemot gram fct_cat_bin (snd gram.reglesf.(i)))
        suivants 
      ) in
      avoir := ajouteplein vraisuivants q;
      dejavu := t::(!dejavu);
    );
  done;
  Array.of_list (List.rev (!dejavu))

