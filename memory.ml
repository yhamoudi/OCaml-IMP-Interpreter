open Types
exception VarUnknown
exception FunUnknown

(********************************)
(*
    ----------------------------
    CONTENU du fichier memory.ml
    ----------------------------

 - Gestion de la mémoire (de type mem) : la mémoire est une liste de couples (adresse, valeur stockée à cette adresse)
 - Gestion des fonctions : l'ensemble des fonctions définies au début d'un programme est géré dans une "fmémoire" de type fmem

*)
(********************************)

(* Gestion de la mémoire *)
(*************************)

type mem = (addr * int) list (* la mémoire est une liste de couples (adresse, valeur stockée à cette adresse) *)

let rec lit memo addre = match memo with (* renvoie la valeur figurant à l'adresse addre*)
    | [] -> raise VarUnknown
    | (x,y)::q -> if x=addre 
                  then y 
                  else lit q addre

let rec ecrit memo addre valeur = match memo with (* assigne la valeur valeur à l'adresse addre *)
    | [] -> []
    | (x,y)::q -> if x=addre 
                  then (x,valeur)::q 
                  else (x,y)::(ecrit q addre valeur)
    
let rec zeromem memo = match memo with (* remet à zero toutes les valeurs contenues dans la mémoire *)
    | [] -> []
    | (x,y)::q -> (x,0)::(zeromem q)

let rec insert x l=match l with (* insert x dans la liste l en faisant attention aux doublons *)
    | [] -> [x]
    | t::q -> if t=x then l
                     else t::(insert x q)

let build_mem fc =
    let rec buildm c memo=match c with (* initialise la mémoire avec toutes les variables mentionnées dans la commande c à 0 *)
        | Aff (x ,a) -> insert (x,0) memo            
        | Print x -> memo
        | Seq (c1,c2) -> buildm c2 (buildm c1 memo)
        | While (b,c) -> buildm c memo    
        | Ifte (b,c1,c2) -> buildm c2 (buildm c1 memo)
        | Skip -> memo
        | Return a -> memo
  in let rec buildmf fc memo=match fc with
        | EndFunDef c -> buildm c memo
        | FunDef (f,x1,x2,c) -> buildm c memo
        | SeqFunDef ((f,x1,x2,c),fc') -> buildmf fc' (buildm c memo)
  in (("HiddenVar",0)::buildmf fc []) (* on ajoute HiddenVar ppur gérer les fonctions à 0 et 1 arguments*)


(* Gestion des fonctions *)
(*************************)

(* une fonction est définie par : un nom, le nom donné à son premier argument dans la déclaration de la fonction, le nom donné au deuxième argument, la commande qui est le corps de la fonction *)

type fmem = (fname*fvar*fvar*com) list (* les fonctions en mémoire *)

let ecrit_fonction fmemo f x0 x1 c = (* ajoute une fonction *)
    (f,x0,x1,c)::fmemo

let rec lit_fonction fmemo f=match fmemo with (* renvoie un 4-uplet définissant la fonction f *)
    | [] -> raise FunUnknown
    | (g,x0,x1,c)::q -> if g=f then (g,x0,x1,c)
                               else lit_fonction q f 

(* pour fc de type fcom, construit la mémoire des fonctions (de type fmem) initiale et renvoie le corps du programme *)

let build_fmem fc= (* construit la fmémoire initialement et renvoie le corps du pg et la fmemo *)
    let rec buildf fmemo fc=match fc with
        | EndFunDef c -> (c,fmemo)
        | FunDef (f,x1,x2,c) -> (Skip,ecrit_fonction fmemo f x1 x2 c)
        | SeqFunDef ((f,x1,x2,c),fc') -> buildf (ecrit_fonction fmemo f x1 x2 c) fc'
    in buildf [] fc

(*Substitution dans une fonction*)
(********************************)

(* les fonctions subst ci-dessous remplacent, dans les expressions et commandes, les noms x0 et x1 par les expressions arithmétiques k0 et k1*)
(* en pratique, k0=Const i0 et k1=Const i1 où i0 et i1 sont des entiers*)

let rec subst_aexpr a x0 x1 k0 k1=match a with
  | Const k         -> Const k
  | Var x           -> (if x=x0 
                        then k0 
                        else if x=x1 
                             then k1 
                             else Var x)
  | Add(a1,a2)      -> Add (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
  | Min(a1,a2)      -> Min (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
  | Mul(a1,a2)      -> Mul (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
  | Div(a1,a2)      -> Div (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
  | FunApp(f,a1,a2) -> FunApp (f, subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)

let rec subst_bexpr b x0 x1 k0 k1=match b with
    | Boolean b0  ->  Boolean b0
    | Not b0      -> Not (subst_bexpr b0 x0 x1 k0 k1)
    | Equ (a1,a2) -> Equ (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
    | Inf (a1,a2) -> Inf (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
    | Sup (a1,a2) -> Sup (subst_aexpr a1 x0 x1 k0 k1, subst_aexpr a2 x0 x1 k0 k1)
    | And (b1,b2) -> And (subst_bexpr b1 x0 x1 k0 k1, subst_bexpr b2 x0 x1 k0 k1)
    | Or (b1,b2)  ->  Or  (subst_bexpr b1 x0 x1 k0 k1, subst_bexpr b2 x0 x1 k0 k1)

let rec subst_com c x0 x1 k0 k1= match c with
    | Aff (x ,a)     -> Aff (x, subst_aexpr a x0 x1 k0 k1)        
    | Print x        -> Print x
    | Seq (c1,c2)    -> Seq (subst_com c1 x0 x1 k0 k1, subst_com c2 x0 x1 k0 k1)
    | While (b,c)    -> While (subst_bexpr b x0 x1 k0 k1, subst_com c x0 x1 k0 k1)
    | Ifte (b,c1,c2) -> Ifte (subst_bexpr b x0 x1 k0 k1, subst_com c1 x0 x1 k0 k1, subst_com c2 x0 x1 k0 k1)
    | Skip           -> Skip
    | Return a       -> Return (subst_aexpr a x0 x1 k0 k1)

(* pour une fonction f avec k0 et k1 en argument : renvoie la commande corps de la fonction, où les arguments x0 et x1 de la fonction sont remplacés par k0 et k1 *)
(* on veille à ne pas remplacer les "HiddenVar", qui permettent de gérer les fonctions à 0 ou 1 arguments *)
let subst_fonction fmemo cf x0 x1 k0 k1=
        if x0="HiddenVar"
        then if x1="HiddenVar"
             then cf
             else subst_com cf x0 x1 (Var "HiddenVar") k1
        else if x1="HiddenVar"
             then subst_com cf x0 x1 k0 (Var "HiddenVar")
             else subst_com cf x0 x1 k0 k1
