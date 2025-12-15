/* start*/
/*start: ouvrir e ecrire le plateau sans arguments*/
start:- open('Plateau.txt',read,Plateaustr)
,lireetat(Plateaustr,Plateau),close(Plateaustr),printplateau(Plateau).

/* le but de les predicats lireetat e savueretat n'est pas de être appelé avec le queuries (?-),
mais fonccioner comme un variable global pour manipuler le plateau sans bésoin d'un loop global et communiquer avec
le frontend c++
*/
/*lireetat(+Plateaustr, -Plateau)*/
/*retour du plateau comme une liste*/
lireetat(Plateaustr,[]):- at_end_of_stream(Plateaustr),!.
lireetat(Plateaustr,[X|R]):-
     \+(at_end_of_stream(Plateaustr)),
     
    read(Plateaustr,X),
    lireetat(Plateaustr,R).
/*sauveretat(+Plateau)
ouvrir le txt et appeler sauveretatformat*/
sauveretat(Plateau):-
    open('Plateau.txt',write,Plateaustr),
    sauveretatformat(Plateaustr,Plateau,0),
    close(Plateaustr).
    /*write(sauve)*/

/*sauveretatformat(+Plateaustr,+Plateau,i N)
écrire le état du plateau au txt dans le format correct*/
sauveretatformat(_,_,25):-!.
sauveretatformat(_,[],_):-!.
sauveretatformat(Plateaustr,[X|R],N):-
    write(Plateaustr,X),
    write(Plateaustr,'.'),
    nl(Plateaustr),
    N2 is N+1, 
    sauveretatformat(Plateaustr,R,N2).


/*printplateau(+Plateau)*/
/*écrire l'etat du plateau au terminal dans un format lisible 5 x 5*/
/*joker au fin pour jeter le end_of_file*/
printplateau([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,V,U,W,X,Y,_]):- nl,write([A,B,C,D,E]), nl, write([F,G,H,I,J]),nl,write([K,L,M,N,O]),nl,write([P,Q,R,S,T]),nl,write([U,V,W,X,Y]).

/*plateauplein(+Plateau)
Retour true ssi le plateau est plein 
(v = case vide) n'appartient pas au plateau*/
plateauplein(Plateau):- \+ member(v,Plateau).

/*retourligne(+Plateau,-Ligne,-Plateau2, i N)
retour de la prémiere ligne au Plateau reçu comme une liste
retour aussi du reste du plateau*/
retourligne(Plateau,[],Plateau,5):-!.
retourligne([X|Rplateau],[X|RLigne],Plateau2,N):- N2 is N+1, retourligne(Rplateau, RLigne,Plateau2,N2).

/*checkvictoire(+Couleur)*/
/*Check de toutes les victoires possibles*/
checkVictoire(Plateau, C):- notrace,
(checkVHorPlateau(C, Plateau);checkVCarPlateau(C,Plateau);checkVVerPlateau(C,Plateau,0);checkVDiagPlateauM1(C,Plateau,0);checkVDiagPlateauP1(C,Plateau,0))
.

/*checkVictoireHorizontalPlateau(+Couleur,+Plateau)*/
/*check la victoire horizontal (4 pions de la même couleur en sequence ) du plateau entier ligne par ligne*/
checkVHorPlateau(C, Plateau):- retourligne(Plateau,Ligne,Plateau2,0), ( checkVhor(C,Ligne) -> !;checkVHorPlateau(C,Plateau2)).
/*checkVictoireHorizontal(+Couleur,+Ligne) et chechVhor2
check la victoire horizontal en 1 ligne (auxiliaire du predicat en haut)*/
checkVhor(C, [C|R]):- checkVhor2(C,R,0),!.
checkVhor(C, [_|R]):- checkVhor(C,R).
checkVhor2(_,_,3).
checkVhor2(C,[C|R],N):- N2 is N+1, checkVhor2(C,R,N2).

/*checkVictoireCarrePlateau(+Couleur, +Plateau)
Check du plateau ligne par ligne si il y a une victoire en carré (Bloc de 4 pions de la même couleur)
*/
checkVCarPlateau(C, Plateau):- retourligne(Plateau,Ligne,Plateau2,0), 
(checkVCar(C,Ligne,NL) ->  retourligne(Plateau2, Ligne2,_,0), checkVCarBas(C,Ligne2,NL)
;checkVCarPlateau(C,Plateau2)).
/*checkVictoireCarre(+Couleur, +Ligne, -N)
check si il y a 2 pions de la même couleur en sequence, et retour l'index N du premier */
checkVCar(C, [C|R],0):- checkVCar2(C, R). 
checkVCar(C, [_|R],N):- checkVCar(C, R,N2), N is N2+1. 
checkVCar2(C,[C|_]).
/*checkVictoireCarreBas(+Couleur, +Ligne,+N)
check si dans le même index de la ligne en haut, il y a une pion de la même couleur(suivi par un autre pion de la même couleur) */
checkVCarBas(C,[C|R],0):- checkVCar2(C,R).
checkVCarBas(C,[_|R],N):- N2 is N-1, checkVCarBas(C,R,N2).


/*checkVictoireDiagonalPlateauPlus1(+Couleur,+Plateau,i Max)
check si il y a une victoire en diagonal avec le numero de colonne des pions plus 1 en relation au antérieur dans les 2 prémieres lignes
ex: [v,b,v,v,v ,v,v,b,v,v ,v,v,v,b,v ,v,v,v,v,b ,v,v,v,v,v]
*/
checkVDiagPlateauP1(C,[v,v,v,v,C],_):-!.
checkVDiagPlateauP1(C,[v,v,v,C,v],_):-!.
checkVDiagPlateauP1(_,_,2):- !, fail.
checkVDiagPlateauP1(C,Plateau,Max):- retourligne(Plateau,Ligne,Plateau2,0), (checkVVer(C,Ligne,NC)->
diagAdd(NC,NC2),checkVDiag(C,Plateau2,NC2),retourligne(Plateau2,_,Plateau3,0), diagAdd(NC2,NC3),checkVDiag(C,Plateau3,NC3), diagAdd(NC3,NC4),retourligne(Plateau3,_,Plateau4,0),checkVDiag(C,Plateau4,NC4)
;Max2 is Max+1, checkVDiagPlateauP1(C,Plateau2,Max2)).
/*diagAdd et diagMoins(+NC,-NC2)
retour NC +1 et verif si est possible de faire une victoire diagonal avec le n de colonne
retour NC-1, verif similaire*/
diagAdd(NC,NC2):- NC2 is NC+1, NC2<6.
diagMoins(NC,NC2):- NC2 is NC-1, NC2> -1.

/*checkVictoireDiagonalPlateauMoins1(+Couleur,+Plateau,i Max)
check dans les 2 prémiéres lignes du plateau si il y a une victoire en diag avec le número de colonne moins 1 en relation au anterieur
Ex:
[v,v,v,v,v 
,v,v,v,b,v
,v,v,b,v,v
,v,b,v,v,v 
,b,v,v,v,v]*/
checkVDiagPlateauM1(_,_,2):- !, fail.
checkVDiagPlateauM1(C,Plateau,Max):- retourligne(Plateau,Ligne,Plateau2,0), (checkVVer(C,Ligne,NC)->
diagMoins(NC,NC2),checkVDiag(C,Plateau2,NC2),retourligne(Plateau2,_,Plateau3,0), diagMoins(NC2,NC3),checkVDiag(C,Plateau3,NC3), diagMoins(NC3,NC4),retourligne(Plateau3,_,Plateau4,0),checkVDiag(C,Plateau4,NC4)
;Max2 is Max+1, checkVDiagPlateauM1(C,Plateau2,Max2)).

/*checKVictoireDiagonal(+Couleur,+Ligne, +NC)
check si dans le numero de colonne reçu, il y a la couleur */
checkVDiag(_,[],5).
checkVDiag(C,[C|_],0):-!.
checkVDiag(C,[_|R],NC):- NC2 is NC-1, checkVDiag(C,R,NC2).

/*checkVictoireVerticalPlateau(+Couleur, +Plateau, i Max)
check si il y a une victoire en vertical dans les 2 prémieres lignes
(il est impossible de faire une victoire verticale sans les 2 prémeieres lignes)
*/
checkVVerPlateau(_,_,2):- !,fail.
checkVVerPlateau(C,Plateau,Max):- retourligne(Plateau,Ligne,Plateau2,0), 
(checkVVer(C,Ligne,NC) -> 
checkVVer2(C,Plateau2,NC), 
retourligne(Plateau2,Ligne3,Plateau4,0),checkVVer2(C,Ligne3,NC),checkVVer2(C,Plateau4,NC); Max2 is Max+1, checkVVerPlateau(C,Plateau2,Max2)).
/*checkVictoireVertical(+Couleur,+Ligne,-Numero colonne)
Check si il y a un pion de la couleur et retour du número de la colonne
aux pŕedicat checkVVerPlateau  */
checkVVer(C,[C|_],0):-!.
checkVVer(C,[_|R],N):- checkVVer(C,R,N2), N is N2+1.
/*checkVVer2(C,Ligne,NC)
check si dans le número de colonne reçu il y a le bon pion*/
checkVVer2(C,[C|_],0):-!.
checkVVer2(C,[_|R],NC):- NC2 is NC-1, checkVVer2(C,R, NC2).

/*La IA est toujours bleu
Le joueur est toujours rouge*/
evalPlateau([],Val):- Val is 0,!.
evalPlateau(Plateau,Val):- checkVictoire(Plateau, b), Val is 1,!.
evalPlateau(Plateau,Val):- checkVictoire(Plateau, r), Val is -1,!.


coupP1(C,[v|R],[C|R]).
coupP1(C,[X|R],[X|R2]):-coupP1(C,R,R2).  


coupP2e(C,[C|R],[v|R],0).
coupP2e(C,[X|R],[X|R2],NC):- coupP2e(C,R,R2,NC2),NC is NC2+1.


coupP2p(C,[v|R],[C|R],NC,NT):- (NT is NC+1, NT<5;NT is NC-1,NT> -1).
coupP2p(C,[X|R],[X|R2],NC,NT):-  NT2 is NT+1,coupP2p(C,R,R2,NC,NT2).

coupP2PSeparer(_,[],[]).
coupP2PSeparer(C,[[NC,Ligne]|R], [Coup|R2]):- findall(Coup,coupP2p(C,Ligne,Coup,NC,0),Coup),coupP2PSeparer(C,R,R2).


coupP2(C,Ligne,Coups):- (findall([NC,LigneEnleve],coupP2e(C, Ligne, LigneEnleve,NC),LignesEnleves)
->  coupP2PSeparer(C,LignesEnleves, Coups)).

/* recomposerPlateau(+Liste de Coups ,PlateauOG,-Liste de Coups avec le bout du plateau)
mettre la liste de coups au top du plateau et retour d'une liste de liste
*/
recomposerPlateau([],POG,[v,v,v,v,v|POG]).  
recomposerPlateau(L,POG,LC):- reverse(L,LR), recomposerPlateau(LR,POG,LC,POG,0).
recomposerPlateau([],LC,[LC],_,_).
recomposerPlateau(LO,Pcopie,[Pcopie|R],POG,5):- recomposerPlateau(LO,POG,R,POG,0).
recomposerPlateau([T|R],Pcopie,LR,POG,N):- N2 is N+1,recomposerPlateau(R,[T|Pcopie],LR,POG,N2).

appendTeteAuPlateau(_,[],[]).
appendTeteAuPlateau(Tete,[Plateau|R],[Res|RRes]):- appendTeteAuPlateau(Tete,R,RRes), append(Tete,Plateau,Res).

toutCoupsP2hor(C,[v,v,v,v,v|R],TCoups,TPlateau):- toutCoupsP2hor(C,R,TCoups,TPlateau).
toutCoupsP2hor(_,[],[],_):-!.
toutCoupsP2hor(C,Plateau,[CoupsDansPlateau|TCoups],TPlateau):- notrace,retourligne(Plateau,Ligne,Plateau2,0),findall(Coup, coupP2(C,Ligne, Coup),TCoupsL1), flatten(TCoupsL1,CoupsListe),
recomposerPlateau(CoupsListe,Plateau2,CoupsAvecPlateausFin), notrace,
(length(CoupsAvecPlateausFin,1)-> !,flatten(CoupsAvecPlateausFin,CAPF),
append(TPlateau,CAPF,CoupsDansPlateau),
append(TPlateau,Ligne,PlateauTete),toutCoupsP2hor(C,Plateau2,TCoups,PlateauTete);

appendTeteAuPlateau(TPlateau,CoupsAvecPlateausFin,CoupsDansPlateau),
append(TPlateau,Ligne,PlateauTete),toutCoupsP2hor(C,Plateau2,TCoups,PlateauTete)).

toutCoupsP1(C,Plateau,TCoups):- (notrace, findall(Coup, coupP1(C,Plateau, Coup),TCoups)).

toutCoupsP2(C,Plateau,TCoupsUniques) :- toutCoupsP2hor(C,Plateau,TCoupsH,[]),flatten(TCoupsH,TCoupsHF),split2(TCoupsHF,25,TCoupsHS),
TCoups = TCoupsHS,delete(TCoups,Plateau,TCoupsSansPasser),list_to_set(TCoupsSansPasser,TCoupsUniques).

split2([],_,[]) :- !.
split2(Lst, N, [FirstN|Res]) :-
    length(FirstN, N),
    append(FirstN, Rest, Lst), !,
    split2(Rest, N, Res).
split2(Lst, N, Lst) :-
    length(Lst, Len),
    Len < N.
permuterRB(r,b).
permuterRB(b,r).

comparerCoups(b,CoupA,ValA,_,ValB,CoupA,ValA):- ValA>=ValB.
comparerCoups(b,_,ValA,CoupB,ValB,CoupB,ValB):- ValB>ValA.
comparerCoups(r,CoupA,ValA,_,ValB,CoupA,ValA):- ValA=<ValB.
comparerCoups(r,_,ValA,CoupB,ValB,CoupB,ValB):- ValB<ValA.


mCoup(b,[],_,_,-2).
mCoup(r,[],_,_,2).
mCoup(C,[Coup|R],P,MCoup, MVal):- write(Prof),(P>0->!,
(evalPlateau(Coup,V)-> Prof is P-1, mCoup(C,R,Prof,MCoupAc,MValAc), comparerCoups(C,Coup,V,MCoupAc,MValAc,MCoup,MVal);
!,Prof is P-1, mCoup(C, R, Prof,MCoupAc, MValAc) ,permuterRB(C,CAutre), minimax(CAutre,Coup,Prof,_,RAR), 
comparerCoups(C,Coup,RAR, MCoupAc, MValAc, MCoup,MVal)
)
;MCoup = Coup,MVal is 0).



compter([],_,N,N).
compter([C|R],C,N,Q):- N2 is N+1, !, compter(R,C,N2,Q).
compter([_|R],C,N,Q):-compter(R,C,N,Q).

minimax(_,[Coup|_],0,Coup, 0).
minimax(C,Plateau,Prof,MCoup, MVal):- notrace,
not(compter(Plateau,C,0,4))->toutCoupsP1(C,Plateau,TC), mCoup(C,TC,Prof,MCoup,MVal)
;toutCoupsP2(C,Plateau,TC), mCoup(C,TC,Prof,MCoup,MVal).

/*MinimaxINI(+Couleur,+Plateau,-MeilleurCoup)
Initializer l'algorithime minimax pour jouer comme la couleur, dans le plateau, retour et print du meilleur coup
        Profondité de la recherche dans le Jeu: ↓*/
minimaxINI(C,Plateau,MCoup):- minimax(C,Plateau,4,MCoup, _),printplateau(MCoup).
/*jouer(+N,+Couleur)
Joue le pion dans l'espace N */
jouer(N,C):- open('Plateau.txt',read,Plateaustr),lireetat(Plateaustr,Plateau),
jouer(N,C,Plateau,L), sauveretat(L), close(Plateaustr),start.

jouer(0,C,[_|R],[C|R]):-!.
jouer(N,C,[T|R],[T|L]):- N>0, N2 is N-1,jouer(N2,C,R,L).