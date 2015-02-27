open Types
open Memory
exception DivBy0
exception ReturnNotFound
exception FunctionMisUsed

(* sémantique opérationnelle à grands pas pour les expr arith*)
(*************************************************************)

(* renvoie un couple (k,m) où k est un entier (a est évalué en k) et m la mémoire après évaluation *)
let rec aeval a memo fmemo= match a with
  | Const k -> (k,memo)
  | Var x -> (lit memo x,memo)    (* on récupère la valeur correspondante en mémoire *)
  | Add(a1,a2) -> let (k1,m1) = aeval a1 memo fmemo in 
                  let (k2,m2) = aeval a2 m1 fmemo in
                    (k1+k2,m2)
  | Min(a1,a2) -> let (k1,m1) = aeval a1 memo fmemo in 
                  let (k2,m2) = aeval a2 m1 fmemo in 
                    (k1-k2,m2)
  | Mul(a1,a2) -> let (k1,m1) = aeval a1 memo fmemo in 
                  let (k2,m2) = aeval a2 m1 fmemo in  
                    (k1*k2,m2)
  | Div(a1,a2) -> begin
                    let (k1,m1) = aeval a1 memo fmemo in 
                    let (k2,m2) = aeval a2 m1 fmemo in  
                        if k2=0 then raise DivBy0
                        else (k1/k2,m2)
                    end
  | FunApp(f,a1,a2) -> feval memo fmemo f a1 a2

(* sémantique opérationnelle à grands pas pour les expr bool*)
(************************************************************)

(* renvoie un couple (b,m) où b est un booléen et m la mémoire après évaluation *)
and beval b memo fmemo= match b with
    | Boolean b0 -> (b0,memo)
    | Not b0 -> let (b,m)= (beval b0 memo fmemo) in (not b, m)
    | Equ (a1,a2) -> let (k1,m1)=(aeval a1 memo fmemo) in
                     let (k2,m2)= (aeval a2 m1 fmemo) in
                        (k1=k2,m2)
    | Inf (a1,a2) -> let (k1,m1)=(aeval a1 memo fmemo) in
                     let (k2,m2)= (aeval a2 m1 fmemo) in
                        (k1<k2,m2)
    | Sup (a1,a2) -> let (k1,m1)=(aeval a1 memo fmemo) in
                     let (k2,m2)= (aeval a2 m1 fmemo) in
                        (k1>k2,m2)
    | And (b1,b2) -> let (bb1,m1)=(beval b1 memo fmemo) in
                     let (bb2,m2)=(beval b2 m1 fmemo) in
                        (bb1 && bb2,m2)
    | Or (b1,b2) -> let (bb1,m1)=(beval b1 memo fmemo) in
                    let (bb2,m2)=(beval b2 m1 fmemo) in
                        (bb1 || bb2,m2)

(* sémantique opérationnelle à petit pas pour les commandes*)
(***********************************************************)

(* en entrée : memo : mémoire
               fmemo : fonctions définies au début du programme
               c : commande
   en sortie : (m,sc) où : sc : None si exécution finie
                                Some c' (avec c'= c après un pas) s'il reste encore à exécuter
                           m : memo après un pas *) 
and pas memo fmemo c = match c with
    | Aff (x ,a) -> let (k,m)=aeval a memo fmemo in (ecrit m x k,Some Skip)
    | Print x -> begin
                    print_string x ; print_string " "; print_int (lit memo x); print_newline(); 
                    (memo, Some Skip)
                 end
    | Seq (c1,c2) -> begin
                        if c1=Skip then (memo,Some c2) 
                                   else let (m,c3)=pas memo fmemo c1 in match c3 with
                                            | Some x -> (m, Some (Seq (x,c2)))
                                            | None -> (m,Some c2)
                     end
    | While (b,c) -> begin
                        let (bb,m)=(beval b memo fmemo) in
                        if bb then (m,Some (Seq(c,While(b,c))))
                              else (m,Some Skip)
                     end
    | Ifte (b,c1,c2) -> begin
                            let (bb,m)=(beval b memo fmemo) in
                            if bb then (m, Some c1)
                                  else (m, Some c2)
                        end
    | Skip -> (memo, None)
    | Return _ -> (memo,None)

(* évaluation d'une fonction *)
(*****************************)

(* pour une fonction f avec k1 et k2 2 expr arith en arguments, renvoie l'entier f(k1,k2) *)

and feval memo fmemo f a1 a2=
    let (_,x1,x2,cf)=lit_fonction fmemo f in
        if ((a1=Var "HiddenVar" && x1<>  "HiddenVar") ||
            (a1<>Var "HiddenVar" && x1= "HiddenVar") ||
            (a2=Var "HiddenVar" && x2<> "HiddenVar") ||
            (a2<>Var "HiddenVar" && x2= "HiddenVar"))
        then raise FunctionMisUsed     (* le nombre d'arguments est incorrect *)
        else
    let (k1,m1)=aeval a1 memo fmemo in
    let (k2,m2)=aeval a2 m1 fmemo in
    let co = subst_fonction fmemo cf x1 x2 (Const k1) (Const k2) in    (* co : commande coeur de la fonction f où Const k1 et Const k2 ont remplacé x0 et x1 (les 2 variables utilisées lors de la déclaration de la fonction en début de programme) *)
    let rec fevalc m c=match c with    (* renvoie un couple (m,k) où m est la mémoire après l'évaluation de la commande c, et k=Some x si un return x a eu lieu dans c, None sinon *) (* dès qu'un return x a lieu, on arrête toute évaluation *)
        | Aff (x ,a) -> let (k,m')=aeval a m fmemo in (ecrit m' x k,None)
        | Print x -> begin
                        print_string x ; print_string " "; print_int (lit m x); print_newline(); 
                        (m, None)
                     end
        | Seq (c1,c2) -> begin
                            if c1=Skip then fevalc m c2
                                       else let (m',k)=fevalc m c1 in match k with
                                        | Some x -> (m',Some x)
                                        | None -> fevalc m' c2
                         end
        | While (b,c) -> begin
                            let (bb,m')=(beval b m fmemo) in
                            if bb then fevalc m' (Seq(c,While(b,c)))
                                  else (m',None)
                          end
        | Ifte (b,c1,c2) -> begin
                                let (bb,m')=(beval b m fmemo) in
                                if bb then fevalc m' c1
                                      else fevalc m' c2
                            end
        | Skip -> (m,None)
        | Return a -> let (k,m')=aeval a m fmemo in (m',Some k)    
    in 
     let (m,k)=fevalc m2 co in match k with
        | Some x -> (x,m)
        | None -> raise ReturnNotFound (* aucun return de trouvé *)

(* Execution du programme *)
(**************************)

(* une fois les fonctions définies dans fmemo et la mémoire initialisée dans memo, on lance exec sur le corps du programme c*)
let rec exec memo fmemo c = (* exécute la commande c dans les états (memo, fmemo) et renvoie memo après exécution *)
        let sc=Some c in
        let rec exec2 memo scm = match scm with
            | None -> memo
            | Some c' -> begin 
                            let (m,scm')=pas memo fmemo c' in exec2 m scm'
                         end
          in exec2 memo sc

(* construit les mémoires puis exécute le corps du programme *)
let rec execute fc =
        let (c,fm)=build_fmem fc in
        let _=exec (build_mem fc) fm c in ()
