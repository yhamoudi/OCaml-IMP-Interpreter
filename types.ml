

type addr = string		(* adresse (nom de variable) *)
type fname = string		(* nom de fonction *)
type fvar = string		(* nom d'un argument d'une fonction *)




(* Les différents types utilisés pour les commandes et les expressions arithmétiques et booléennes *)

(* Type pour les expressions arithmétiques *)

type aexpr =
  | Const of int				(* constante entiere *)
  | Var of addr					(* variable *)
  | Add of aexpr*aexpr			(* addition *)
  | Min of aexpr*aexpr			(* soustraction *)
  | Mul of aexpr*aexpr  		(* multiplication *)
  | Div of aexpr*aexpr			(* division *)
  | FunApp of fname*aexpr*aexpr	(* application d'une fonction dont les arguments sont des expressions arithmétiques *)
  
  
(* Type pour les expressions booléennes *)

type bexpr = 
	| Boolean of bool		(* constante booléenne *)
	| Not of bexpr			(* négation *)
	| Equ of aexpr*aexpr	(* égalité *)
	| Inf of aexpr*aexpr	(* inférieur *)
	| Sup of aexpr*aexpr	(* supérieur *)
	| And of bexpr*bexpr	(* et *)
	| Or of bexpr*bexpr		(* ou *)
	
	
(* Type pour les commandes *)

type com = 
  | While of bexpr*com			(* while *)
  | Ifte of bexpr*com*com		(* if then else *)
  | Seq of com*com				(* séquence *)
  | Aff of addr*aexpr			(* affectation *)
  | Print of addr				(* print *)
  | Skip						(* skip *)
  | Return of aexpr				(* return, à n'utiliser que lors de la définition d'une fonction *)

(* Type pour des définitions de fonctions, suivies du corps du programme *)

 (* une fonction = nom : fname, arguments : fvar et fvar, corps de la fonction : com *)
type fcom=
  | FunDef of fname*fvar*fvar*com			(* définitin d'une fonction seule (pas d'intérêt) *)
  | SeqFunDef of (fname*fvar*fvar*com)*fcom	(* définition d'une fonction, puis suite du fichier *)
  | EndFunDef of com						(* corps du programme (fin des définitions de fonctions) *)




