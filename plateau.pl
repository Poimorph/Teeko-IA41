


start:- open('Plateau.txt',read,Plateaustr)
,lireetat(Plateaustr,Plateau),close(Plateaustr),printplateau(Plateau).

/* le but de les predicats lireetat e savueretat n'est pas de être appelé avec le queuries (?-),
mais fonccioner comme un variable global pour manipuler le plateau sans bésoin d'un loop global et communiquer avec
le frontend c++
*/
lireetat(Plateaustr,[]):- at_end_of_stream(Plateaustr),!.
lireetat(Plateaustr,[X|R]):-
     \+(at_end_of_stream(Plateaustr)),
     
    read(Plateaustr,X),
    lireetat(Plateaustr,R).

sauveretat(Plateau):-
    open('Plateau.txt',write,Plateaustr),
    sauveretatformat(Plateaustr,Plateau,0),
    close(Plateaustr),
    write(sauve).

sauveretatformat(_,_,25):-!.
sauveretatformat(_,[],_):-!.
sauveretatformat(Plateaustr,[X|R],N):-
    write(Plateaustr,X),
    write(Plateaustr,'.'),
    nl(Plateaustr),
    N2 is N+1,
    sauveretatformat(Plateaustr,R,N2).


%joker au fin pour jeter le end_of_file
printplateau([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,V,U,W,X,Y,_]):- write([A,B,C,D,E]), nl, write([F,G,H,I,J]),nl,write([K,L,M,N,O]),nl,write([P,Q,R,S,T]),nl,write([U,V,W,X,Y]).

jouer(N,Couleur):- open('Plateau.txt',read,Plateaustr),lireetat(Plateaustr,Plateau), 
jouer(N,Couleur,Plateau,L), sauveretat(L).

jouer(0,Couleur,[_|R],[Couleur|R]):-!.
jouer(N,Couleur,[T|R],[T|L]):- N>0, N2 is N-1,jouer(N2,Couleur,R,L).
