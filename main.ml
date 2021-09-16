(*le type e doit contenir les terminaux ET les non terminaux*)

type 'a regle = 'a * ('a list);;

type 'e grammaire = {terminaux : 'e list ; nonterminaux : 'e list ; axiome : 'e ; regles : string regle list };;

type 'e preuve_appartenance = A of 'e | P of 'e * ('e preuve_appartenance list) * 'e regle;;


(*Exemples*)

let g_op_suffixe = {terminaux = ["+";"-";"*";"0";"1";"2";"3";"4";"5";"6";"7";"8";"9"];nonterminaux = ["X"];axiome="X";
    regles=[
        ("X",["X";"X";"+"]);
        ("X",["X";"X";"*"]);
        ("X",["X";"x";"-"]);
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
    ]};;


(*Prouvons que n est dans L*)
let preuve n = P ( "X", [A "X"], ("X", [string_of_int n]) );;

(*Prouvons que 36* est dans L*)
let preuve2 = P ( "X", [preuve 3;preuve 6], ("X",["X";"X";"*"]));;
(*Prouvons que 236*+ est dans L*)
let preuve3 = P ( "X", [preuve 2;preuve2], ("X",["X";"X";"+"]));;

(*Prouvons que 16* est dans L*)
let preuve4 = P ( "X", [preuve 1;preuve 6], ("X",["X";"X";"*"]));;
(*Prouvons que 16*7+ est dans L*)
let preuve5 = P ( "X", [preuve4;preuve 7], ("X",["X";"X";"+"]));;

(*Prouvons que 236*+16*7+* est dans L*)
let preuvefinale = P ( "X", [preuve3;preuve5], ("X",["X";"X";"*"]));;

let rec verif_preuve g p = match p with
    |A x -> g.axiome = x
    |P (x,l,r) -> (not (List.mem false (List.map (verif_preuve g) l) )) && (fst r = x) && (List.mem r g.regles);;

verif_preuve g_op_suffixe (preuvefinale);;

let rec resultat_preuve g p = match p with
	|A x -> ""
	|P (x,l,r) ->
		let r1,r2 = r in 
		let nl = ref l in
		List.fold_right
			(fun x y -> x^y)
			(
				List.map
					(
					fun x ->
					if List.mem x g.nonterminaux then
						if (r1 = x) 
							then
								(
								let t,q = (List.hd (!nl),List.tl (!nl)) in
								nl:=q;
								resultat_preuve g t
								) 
							else
								failwith "eh oh elle va pas ta grammaire la"
						else
						x
					)
				r2
			)
			""
		;;

resultat_preuve g_op_suffixe (preuvefinale);;