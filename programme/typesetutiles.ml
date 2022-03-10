(*##### TYPES #####*)

type 'e caractere = T of 'e | Nt of int

type 'a regle = ('a array) * ('a array)

type 'e fg = {
  terminaux : ('e caractere) array ;
  nonterminaux : ('e caractere) array ;
  axiome : 'e caractere ;
  reglesf : ('e caractere) regle array
} 

type 'a reglecf = int * ('a list)

type 'e cfg = {
  terminaux : ('e caractere) array ;
  nonterminaux : ('e caractere) array ;
  axiome : 'e caractere ;
  reglescf : ('e caractere) reglecf array
}

type 'a arbre = 'a * 'a foret and 'a foret = F of 'a arbre list





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

let lisracines foret = match foret with
    |F l -> List.map fst l

(* Effectue un pretraitement (kmp) des membres de gauche des règles de dérivation *)
let preprocessgf grf =
  Array.map 
  (fun (a,b) ->
    (a,b,kmppreprocess a) 
  )
  grf.reglesf 
