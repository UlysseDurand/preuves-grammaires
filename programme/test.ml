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
