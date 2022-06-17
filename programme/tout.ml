(*##### TYPES #####*)

type 'e caractere = T of 'e | Nt of int

type 'a regle = ('a array) * ('a array)

type 'e fg = {
  terminaux : ('e caractere) array ;
  nbnonterminaux : int ;
  axiome : 'e caractere ;
  reglesf : ('e caractere) regle array
} 

type 'e preuveformelle = (('e caractere list)*int*int) list



(*##### UTILES #####*)

(* Donne le trableau des lettres de la grammaire *)
let lettres gram = Array.append gram.terminaux (Array.mapi (fun i x -> Nt i) (Array.make (gram.nbnonterminaux) 0))

let implies a b = (not a) || b

(* Genere tous les n uplets dans {0,1}*)
let rec nuplets n =
  if n = 1 then [[0];[1]]
  else
  let autre = nuplets (n-1) in
  (List.map (fun l -> 0::l) autre )@( List.map (fun l -> 1::l) autre)

(* Produit cartesien *)
let cartesian l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) l') l)

let enarray x = (List.map Array.of_list x)


(* Implementation de kmp *)
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

(* remplace x i l b remplace dans x le sous mot de longueur l qui commence a l'indice i par le mot b *)
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

(* Effectue un pretraitement (kmp) des membres de gauche des regles de derivation *)
let preprocessgf grf =
  Array.map 
  (fun (a,b) ->
    (a,b,kmppreprocess a) 
  )
  grf.reglesf 

let nboccur m l = List.length ((List.filter (fun x -> x = l) ) (Array.to_list m))

(* Parcours en largeur *)
let rec bfs g dejaVus aVoir = 
  match aVoir with
    |[] -> dejaVus
    |tete::queue -> if List.mem tete dejaVus
      then bfs g dejaVus queue
      else bfs g (tete::dejaVus) (queue@(g tete));;

(* Donne le nombre d'occurences des lettres de la grammaire dans le mot *)
let analysemot mot gram = 
	let n = gram.nbnonterminaux in
	let m = Array.length gram.terminaux in
	let res = Array.make (n+m) 0 in
	Array.mapi (fun i l -> if i < n then nboccur mot (Nt i) else nboccur mot (gram.terminaux.(i-n)) ) res

let ordre a b = (Array.length a <= Array.length b ) && (
    let res = ref true in
    for i=0 to ((Array.length a ) - 1) do
    	if b.(i) < a.(i) then res:=false;
    done;
    !res
  )

let moins a b = 
 let res = Array.make (Array.length a) 0 in
 for i=0 to ((Array.length a) - 1) do
   res.(i) <- a.(i)-b.(i);
 done;
 res

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

(* Retourne les mots vers lesquels x peut deriver une fois *)
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

(* Retourne les mots vers lesquels x peut deriver une fois et comment il derive *)
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

(* Cherche une derivation de x vers m *)
let recherchesuitemots x m ppregles = 
  parcoursmagique
  (succ ppregles)
  (fun x -> false)
  (fun x -> x = m)
  []
  [[x]]

(* La meme fonction mais fourni une preuve dans le type preuveformelle *)
let recherchepreuvenaif gram ppregles x m =
  parcoursmagique
  (succbis ppregles)
  (fun (y,a,b) -> false)
  (fun (y,a,b) -> y = m)
  []
  [[x,0,0]]
   
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
  
  
  
(*##### IMPLEMENTATION DU CODE #####*)

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

let exreglespp = preprocessgf exfg

let m = [|T 'a' ; T 'k' ; T 'k' ; T 'c' ; T 'c' ; T 'k' ; T 'a' ; T 'a' ; T 'a' ; T 'k' ; T 'c' ; T 'k'|]

let x = [|T 'a' ; T 'a' ; T 'a' ; T 'b' ; T 'a' ; T 'k' ; T 'a' ; T 'b'|]

let reszero = interditfort exfg m x

let resun = recherchepreuvenaif exfg exreglespp [|Nt 0|] [|T 'a' ; T 'a' ; T 'a' ; T 'k' ; T 'a' ; T 'a' ; T 'a' ; T 'k'|]

let resdeux = recherchepreuve exfg exreglespp [|Nt 0|] [|T 'a' ; T 'a' ; T 'a' ; T 'k' ; T 'a' ; T 'a' ; T 'a' ; T 'k'|]
