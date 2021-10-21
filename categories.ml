let rec produit_cart l1 l2 =
  match (l1,l2) with 
    |([],_) -> []
    |(_,[]) -> []
    |(t1::q1,t2::q2) -> (t1,t2)::((produit_cart q1 l2)@(produit_cart l1 q2))
;;

type 'a arrow = A of 'a | C of 'a * 'a arrow;; 
(*Un élément de 'a ou une composition d'éléments de 'a*)

let rec compose f g = match (f,g) with
  |(C (t,q),g) -> C (t,compose q g)
  |(A a,g) -> C (a,g)
;;

let implies a b = not(a) || b;;



type ('a,'b) category = ('a list) * ('b list) * ('b -> 'a) * ('b -> 'a);;
(* 
  (obj,arr,dom,cod) : 
    obj l'ensemble des objets,
    arr l'ensemble des flèches, 
    dom et cod les fonctions domaine et codomaine
*)

let check_category c = 
  let (obj,arr,dom,cod) = c in
  let prod = produit_cart arr arr in
  (
    List.for_all
    (
      fun (f,g) -> 
        implies
        (cod f = dom g)
        (List.mem (compose f g) arr)
    ) 
    prod
  )
  &&
  (
    List.for_all
    (
      fun x ->
        List.exists
        (fun a -> (dom a = x && cod a = x))
        arr
    )
    obj
  )
;;

