PROGRAMMATION - DM2 : Interprète IMP
====================================

### Yassine Hamoudi

## SOMMAIRE

 * 0 - Exécution de l'interprète
 * 1 - Programmes tests
 * 2 - Messages d'erreurs
 * 3 - Règles de priorité
 * 4 - Fonctions


0 - Exécution de l'interprète
-----------------------------------------------------

Pour compiler, entrer :
```bash
make
```

Pour exécuter un programme écrit dans le fichier prg, entrer : 
```bash
./imp < prg
```


1 - Programmes tests
-----------------------------------------------------

Des programmes tests figurent dans le dossier tests. Pour exécuter prime.imp par exemple, entrer : 
```bash
./imp < tests/prime.imp
```


2 - Messages d'erreurs
-----------------------------------------------------

Ci-dessous, les explications de quelques messages d'erreur courants : 

 - _Fatal error: exception Parsing.Parse_error_     -> le programme contient une erreur syntaxique
 - _Fatal error: exception Execute.ReturnNotFound_  -> une fonction n'a retourné aucun entier
 - _Fatal error: exception Execute.FunctionMisUsed_ -> un nombre incorrect d'arguments a été utilisé dans une fonction
 - _Fatal error: exception Execute.DivBy0_          -> une division par 0 est survenue
 - _Fatal error: exception Memory.FunUnknown_       -> tentative d'utilisation d'une fonction non définie
 - _Fatal error: exception Memory.VarUnknown_       -> tentative d'utilisation d'une variable non définie


3 - Règles de priorité
-----------------------------------------------------

Quelques règles de priorité choisies sont rappelées ci-dessous.

### Séquence

La séquence est associative à gauche : 
```
c1;c2;c3 == (c1;c2);c3
```

On a précédence(while)>précédence(séquence) : 
```
while b do c1;c2 == (while b do c1);c2
```

On a précédence(conditionnelle)>précédence(séquence) : 
```
if b then c1 else c2;c3 == (if b then c1 else c2); c3
```

### Conditionnelle

On a précédence(THEN)<précédence(IF)<précédence(ELSE) : 
```
if b1 then if b2 then c1 else c2 == if b1 then (if b2 then c1 else c2)
```

### AND, OR et NOT

AND et OR sont associatifs à gauche : 
```
b1 and b2 and b3 == (b1 and b2) and b3
```

On a : précédence(OR)<précédence(AND)<précédence(NOT) : 
```
b1 and b2 or b3 == (b1 and b2) or b3
not not b1 and b2 == (not not b1) and b2
```


4 - Fonctions
-----------------------------------------------------

### Procédure d'utilisation

Les fonctions doivent être définies au début du programme.
Une fonction est de la forme : 
```
function fname(ARG1,ARG2)=
    c.
```

(le point (.) ne doit pas être oublié après la commande c)

où :
 - fname : nom de la fonction (en minuscule)
 - ARG1 et ARG 2 : arguments de la fonction (en majuscule)
 - c : commande constituant le corps de la fonction

Les fonctions doivent comporter 0, 1 ou 2 arguments.

Voici un exemple de programme contenant 2 fonctions : 
```
function identite(X)=
    return X.

function ecart(X,Y)=
    if X>Y
    then return X-Y
    else return Y-X.

W:=identite(ecart(3,7));
print W
```

### Explications du fonctionnement

- Lors d'une déclaration de fonction à 1 ou 0 arguments, on crée 1 ou 2 arguments artificiels supplémentaires (nommés "HiddenVar") pour obtenir une fonction à 2 arguments.
- Lorsqu'une fonction à 1 ou 0 arguments est appellées, on lui ajoute 1 ou 2 Var "HiddenVar" en arguments pour obtenir un appel de fonction avec 2 arguments.

La gestion des fonctions se fait de la manière suivante : 

  * I - le début du programme donné en entrée est parcouru et l'ensemble des fonctions définies sont stockées sous forme de 4-uplets dans une liste. Un 4-uplet (f,x0,x1,c) contient le nom de la fonction (f), ses deux arguments (x0 et x1) et son corps (c).

  * II - lorsqu'une fonction est appelée avec en argument les expressions arithmétiques a0 et a1 :
    - 1 - on récupère la description (f,x0,x1,c) de la fonction stockée en mémoire
    - 2 - on vérifie que si a0=Var "HiddenVar" alors x0="HiddenVar", et réciproquement. On fait de même pour a1 et x1. Si ce n'est pas le cas, on a détecté une erreur dans le nombre d'arguments et on lève une exception.
    - 3 - a0 et a1 sont évalués en 2 entiers k0 et k1
    - 4 - on remplace partout dans c : x0 par k0 et x1 par k1.
    - 5 - on évalue c, en prenant garde de stopper l'évaluation dès qu'un return est atteint

- L'étape I est effectuée par la fonction build_fmem située dans memory.ml
- L'étape II-1 est effectuée par la fonction lit_memo située dans memory.ml
- L'étape II-2 est effectuée au début de la fonction feval située dans execute.ml
- L'étape II-3 est effectuée dans la fonction feval située dans execute.ml
- L'étape II-4 est effectuée par la fonction subst_fonction située dans memory.ml
- L'étape II-5 est effectuée dans la fonction feval située dans execute.ml

