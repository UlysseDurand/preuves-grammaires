

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