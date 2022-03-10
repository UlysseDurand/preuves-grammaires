open Typesetutiles
open Preuveautomatique
open Verifalge

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
let unmachin = succ (preprocessgf exfg) [|T 'a' ; T 'b' ; T 'c' |]

let resultat = chercherderivationnaif [|Nt 0|] [|T 'a' ; T 'a' ; T 'a' ; T 'k'|] (preprocessgf exfg)