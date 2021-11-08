type v_liee = Vl of char 
and formule =  
  Forall of v_liee*formule |
  Exist of v_liee*formule |
  Fbool of element -> bool
;;