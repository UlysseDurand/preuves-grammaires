let indicedansarray e a = 
	let res = ref (-1) in
	for i = 0 to (Array.length a) - 1 do
		if a.(i)=e then res:=i;
	done;
	!res
;;

(*le type e doit contenir les terminaux ET les non terminaux*)

type 'e caractere = T of 'e | Nt of int

type 'a regle_contexte = ('a list) * ('a list);;

type 'a regle = ('a ) * ('a list);;

type 'e grammairemoche = {terminaux : 'e array ; nonterminaux : 'e array ; axiome : 'e caractere ; regles : ('e caractere) regle array };;

type 'e grammairebelle = {terminaux : 'e array ; nonterminaux : 'e array ; axiome : 'e ; joliregles : 'e regle array };;

type 'e preuve_appartenance = A of ('e caractere) | P of ('e caractere) * ('e preuve_appartenance list) * ('e caractere) regle;;

let rendmoche gr = 
	{
		terminaux = gr.terminaux ; 
		nonterminaux = gr.nonterminaux ; 
		axiome = (Nt (indicedansarray gr.axiome gr.nonterminaux)) ; 
		regles = 
			Array.map 
			(fun (i,x) ->
				let j = indicedansarray i gr.nonterminaux in
				(Nt j,
					List.map
					(fun y ->
						let id = (indicedansarray y gr.nonterminaux) in 
						if id=(-1) 
							then (T y)
							else (Nt id)
					)
					x
				)
			)
			gr.joliregles;
	}
;;
(*Exemples*)

let g_op_suffixe = 
	{
		terminaux = [|"+";"-";"*";"0";"1";"2";"3";"4";"5";"6";"7";"8";"9"|];
		nonterminaux = [|"X"|];
		axiome="X";
    joliregles=[|
        ("X",["X";"X";"+"]);
        ("X",["X";"X";"*"]);
        ("X",["X";"X";"-"]);
        ("X",["0"]);
        ("X",["1"]);
        ("X",["2"]);
        ("X",["3"]);
        ("X",["4"]);
        ("X",["5"]);
        ("X",["6"]);
        ("X",["7"]);
        ("X",["8"]);
        ("X",["9"])
    |]
		}
;;

let g_op_suffixe_moche = rendmoche g_op_suffixe;;



(*Prouvons que n est dans L*)
let preuve n = P ( Nt 0, [A (Nt 0)], (Nt 0, [T (string_of_int n)]) );;

(*Prouvons que 36* est dans L*)
let preuve2 = P ( (Nt 0), [preuve 3;preuve 6], ((Nt 0),[(Nt 0);(Nt 0);T "*"]));;
(*Prouvons que 236*+ est dans L*)
let preuve3 = P ( (Nt 0), [preuve 2;preuve2], ((Nt 0),[(Nt 0);(Nt 0);T "+"]));;

(*Prouvons que 16* est dans L*)
let preuve4 = P ( (Nt 0), [preuve 1;preuve 6], ((Nt 0),[(Nt 0);(Nt 0);T "*"]));;
(*Prouvons que 16*7+ est dans L*)
let preuve5 = P ( (Nt 0), [preuve4;preuve 7], ((Nt 0),[(Nt 0);(Nt 0);T "+"]));;

(*Prouvons que 236*+16*7+* est dans L*)
let preuvefinale = P ( (Nt 0), [preuve3;preuve5], ((Nt 0),[(Nt 0);(Nt 0);T "*"]));;

let rec verif_preuve (g : 'e grammairemoche) (p: 'e preuve_appartenance) = 
	match p with
    |A x -> g.axiome = x
    |P (x,l,r) -> 
			(
				List.for_all
				(verif_preuve g)
				l
			)
			&&
			(fst r = x)
			&&
			(Array.mem r g.regles)
;;

verif_preuve g_op_suffixe_moche preuvefinale;;

let rec resultat_preuve g p = 
	match p with
		|A x -> ""
		|P (x,l,r) ->
			let r1,r2 = r in 
			let nl = ref l in
			List.fold_right
			(
				fun x y -> x^y
			)
			(
				List.map
				(
				fun x -> 
					match x with
						|Nt xnt ->  
							if (r1 = x) 
							then
							(
								let t,q = (List.hd (!nl),List.tl (!nl)) in
								nl:=q;
								resultat_preuve g t
							) 
							else
								failwith "eh oh elle va pas ta grammaire la"
						|T xt -> xt
				)
				r2
			)
			""
;;

resultat_preuve g_op_suffixe preuvefinale;;

let rec distance_levenshtein m1 m2 =
	match m1,m2 with
		|_,[] -> List.length m1
		|[],_ -> List.length m2
		|t1::q1,t2::q2 when t1=t2 -> distance_levenshtein q1 q2
		|t1::q1,t2::q2 -> 1 + min ( min (distance_levenshtein q1 m2) (distance_levenshtein m1 q2) ) (distance_levenshtein q1 q2)
;;

distance_levenshtein ["a";"a";"a";"a"] ["b";"b";"b";"b"];;
 
let rec n_cartesian_product = function
  | [] -> [[]]
  | x :: xs ->
    let rest = n_cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
  ;;

n_cartesian_product [[4;7] ; [1;0] ; [3;8]];;

let rec puissance l n =
	n_cartesian_product (Array.to_list (Array.init n (fun x -> l) ) )
;;

puissance [1;2;3] 4;;

(*
r une regle, gr la grammaire et els des elements a combiner.
els sous la forme d'une liste de couples (nonterminal,liste des elements dérivés de ce non terminal.) 
*)
let construit gr r els = 
	let nonterminaux = gr.nonterminaux in
	let besoins = (List.filter (fun x -> match x with |T _ -> false |Nt _ -> true ) (snd r)) in
	let nboccur = Array.init (Array.length nonterminaux) (fun i -> List.length (List.filter (fun y -> y=Nt i) besoins) ) in
	let combinaison_de_preuves = n_cartesian_product (List.map (fun x -> match x with |T _ -> failwith "erreur" |Nt i -> els.(i)) besoins) in
	List.map (fun combi -> P (fst r,combi,r)) combinaison_de_preuves
;;

construit g_op_suffixe ((Nt 0),[(Nt 0);(Nt 0);T "+"]) [|[preuve 2;preuve 3]|];;