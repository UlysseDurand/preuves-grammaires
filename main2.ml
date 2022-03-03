(* ##### UTILE ##### *)

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


(* 
let soustrait m i j = 
  let rec aux unmmot acc parcouru = 
    match unmmot with
      |[] ->List.rev parcouru
      |t::q -> 
        if acc > j then List.rev parcouru
        else aux q (acc+1) (if i >= i then t::parcouru else parcouru)
  in
  aux m 0 [] *)





(* ##### TYPES ##### *)

type 'e caractere = T of 'e | Nt of int

type 'a regle = ('a array) * ('a array)

type 'e fg = {
  terminaux : ('e caractere) array ;
  nonterminaux : ('e caractere) array ;
  axiome : 'e caractere ;
  reglesf : ('e caractere) regle array
} 

type 'a reglecf = int * ('a array)

type 'e cfg = {
  terminaux : ('e caractere) array ;
  nonterminaux : ('e caractere) array ;
  axiome : 'e caractere ;
  reglescf : ('e caractere) reglecf array
}

type 'a arbre = AVide | F of ('a * 'a foret) and
  'a foret = 'a arbre list



(* ##### LE PROGRAMME ##### *)

(* Effectue un pretraitement (kmp) des membres de gauche des règles de dérivation *)
let preprocessgf grf =
  Array.map 
  (fun (a,b) ->
    (a,b,kmppreprocess a) 
  )
  grf.reglesf 

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

(* Un exemple de grammaire formelle *)
let exfg = {
  terminaux = [| T 'a' ; T 'b' ; T 'c' ; T 'k' |];
  nonterminaux = [|Nt 0|];
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

let cetruc = remplace [|0;1;2;3;4;5;6;7;8|] 2 6 [|-1;-2;-3|]

let unmachin = succ (preprocessgf exfg) [|T 'a' ; T 'b' ; T 'c' |]

(* Cherche une dérivation de x vers m *)
let chercherderivation x m ppregles = 
  parcoursmagique
  (succ ppregles)
  (fun x -> false)
  (fun x -> x = m)
  []
  [[x]]

let resultat = chercherderivation [|Nt 0|] [|T 'a' ; T 'a' ; T 'a' ; T 'k'|] (preprocessgf exfg)