(*##### TYPES #####*)

type 'e caractere = T of 'e | Nt of int

type 'a regle = ('a array) * ('a array)

type 'e fg = {
  terminaux : ('e caractere) array ;
  nbnonterminaux : int ;
  axiome : 'e caractere ;
  reglesf : ('e caractere) regle array
} 

type 'e preuveformelle = ('e caractere array) list






(*##### UTILES #####*)

(* Implémentation de kmp *)
let kmppreprocess w =
	let n = Array.length w in
	let pos = ref 1 in
	let cnd = ref 0 in
	let t = Array.make (n+1) (-1) in
	while (!pos) < n do
		if w.(!pos) = w.(!cnd) then
			(t.(!pos) <- t.(!cnd);)
		else
		(
			t.(!pos) <- !cnd;
			while (!cnd >= 0 && w.(!pos) <> w.(!cnd) ) do
				cnd:=t.(!cnd);
			done;
		);
		pos:=(!pos)+1;
		cnd:=(!cnd)+1;
	done;
	t.(!pos) <- (!cnd);
	t;;

let kmp s w t =
	let ns = Array.length s in
	let nw = Array.length w in
	let j = ref 0 in
	let k = ref 0 in
	let res = ref [] in
	while (!j) < ns do
		if w.(!k) = s.(!j) then
		(
			j:=(!j)+1;
			k:=(!k)+1;
			if (!k)=nw then
			(
				res:=((!j)-(!k))::(!res);
				k:=t.(!k);
			);
		)
		else
		(
			k:=t.(!k);
			if (!k) < 0 then
			(
				j:=(!j)+1;
				k:=(!k)+1;
			);
		);
	done;
	!res;;

(*remplace x i l b remplace dans x le sous mot de longueur l qui commence à l'indice i par le mot b*)
let remplace x i l b =
  let n = Array.length x in
  let m = Array.length b in
  if i >= n || n < i+l then failwith "OOH" else
  let res = Array.make (n-l+m) x.(0) in
  for j = 0 to (n-l+m-1) do
    if (j < i) then
    (
      res.(j) <- x.(j)
    ) else
    if (j >= i+m) then
    (
      res.(j) <- x.(j-m+l)
    )
    else
    (
      res.(j) <- b.(j-i)
    );
  done;
  res

let ajoute e l = if List.mem e l then l else e::l

let rec ajouteplein l1 l2 = 
	match l1 with
		|[] -> l2
		|t::q -> (ajouteplein q (ajoute t l2))


(* Effectue un pretraitement (kmp) des membres de gauche des règles de dérivation *)
let preprocessgf grf =
  Array.map 
  (fun (a,b) ->
    (a,b,kmppreprocess a) 
  )
  grf.reglesf 

let nboccur m l = List.length ((List.filter (fun x -> x = l) ) (Array.to_list m))

(* Donne le nombre d'occurences des lettres de la grammaire dans le mot *)
let analysemot mot gram = 
	let n = gram.nbnonterminaux in
	let m = Array.length gram.terminaux in
	let res = Array.make (n+m) 0 in
	Array.mapi (fun i l -> if i < n then nboccur mot (Nt i) else nboccur mot (gram.terminaux.(i-n)) ) res

let categorisemot gram fctcat m = Array.map fctcat (analysemot m gram)

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
  [[x,0,0]](* Fonction catégorisante binaire *)
let fct_cat_bin n = if n > 0 then 1 else 0

(* Vérifie si il existe un mot de catégorie q dérivable via la dérivation d dans la grammaire gram *)
let estpossible q d gram =
  let (a,b) = d in
  let cata = categorisemot gram fct_cat_bin a in 
  q = cata

(* Donne l'ensemble des indices des dérivations faisables depuis l'état q *)
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

(* Donne, pour la dérivation i, l'état-couple vers lequel on arrive *)
let etatsdederiv gram i = 
  let (a,b) = gram.reglesf.(i) in
  (i,categorisemot gram fct_cat_bin b)

(* Donne, les couple de catégories des dérivations d'une grammaire*)
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



(* Un exemple de grammaire formelle *)
let exfg = {
  terminaux = [| T 'a' ; T 'b' ; T 'c' ; T 'k' |];
  nbnonterminaux = 1;
  axiome = Nt 0;
  reglesf = [| 
    [|Nt 0|],[|T 'a' ; T 'b' ; T 'c'|] ;
    [|T 'a' ; T 'b' ; T 'c'|],[|T 'a' ; T 'b'|] ;
    [|T 'b'|],[|T 'k'|] ;
    [|T 'c'|],[|T 'a' ; T 'k'|] ;
    [|T 'k' ; T 'a' ; T 'k'|],[|T 'a' ; T 'a'|] ;
    [|T 'a'|],[|T 'a' ; T 'a' ; T 'a'|]
  |]
}
let unmachin = succ (preprocessgf exfg) [|T 'a' ; T 'b' ; T 'c' |]

let resultat = lememeavecderivations [|Nt 0|] [|T 'a' ; T 'a' ; T 'a' ; T 'k'|] (preprocessgf exfg)

let printcarac c = match c with
  |T c -> print_char c
  |Nt i -> print_int i

let printmot = List.iter printcarac

let pretraited = pretraitebisgram exfg 

let test = estpossible [|0;1;1;1;0|] exfg.reglesf.(3) exfg