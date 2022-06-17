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

