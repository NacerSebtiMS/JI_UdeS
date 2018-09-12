%-----------------------------------------------------------------------------
% Le nom du fichier, le nom du module, le préfixe des prédicats et le nom
% du mutex ont tous la même valeur. Dans ce cas ci, c'est p11. Changez TOUTES
% les occurrences de p11 dans ce fichier pour le préfixe qui vous est assigné.
%-----------------------------------------------------------------------------

% Un JI doit être un module afin d'éviter les conflits de noms entre les JI.
:- module(p11,[p11_nom/1,p11_auteurs/1,p11_reset/0,p11_plan/1,p11_action/2]).

%-----------------------------------------------------------------------------
% Prédicats de jeu.
%-----------------------------------------------------------------------------

% Nom du JI: p11_nom(-Nom)
p11_nom('Feim').

% Auteurs du JI: p11_auteurs(-Auteurs)
p11_auteurs('MAN').

% Remise à zero du JI: p11_reset
p11_reset :-
    planInitial(P),
    setPlan(P).

% Plan courant du JI: p11_plan(-PlanCourant)
p11_plan(Plan) :-
    getPlan(Plan).

% Prochaine action du JI: p11_action(+Etat, -Action)
p11_action(Etat, Action) :-
    trouveAction(Etat, Action).

%-----------------------------------------------------------------------------
% Prédicats internes de plans.
%-----------------------------------------------------------------------------
% La consultation d'un plan et la modification d'un plan sont protégées par
% mutex afin d'éviter les conflits possibles d'appels en parallèle.
%
% Le prédicat planRestant est déclaré dynamique, car sa valeur change au cours
% de l'exécution.
%-----------------------------------------------------------------------------

:- dynamic([planRestant/1]).

planInitial([]).

planRestant([]).

getPlan(Plan) :-
    with_mutex(p11,planRestant(Plan)).

setPlan(Plan) :-
    with_mutex(p11,changePlan(Plan)).

changePlan(Plan) :-
    retractall(planRestant(_)),
    assert(planRestant(Plan)).

%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
%   RECHERCHE EN PROFONDEUR
%-----------------------------------------------------------------------------


trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, EtatJeu = [_,M,_,_,_,_], trouverPlan_Profondeur(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_], setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    EtatJeu = [_,M,_,_,_,_], trouverPlan_Profondeur(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_],[ProchaineAction|PlanRestant]),setPlan(PlanRestant).


%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
%   RECHERCHE EN LARGEUR
%-----------------------------------------------------------------------------
/*

trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, EtatJeu = [_,M,_,_,_,_], trouverPlan_Largeur(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_], setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    EtatJeu = [_,M,_,_,_,_], trouverPlan_Largeur(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_],[ProchaineAction|PlanRestant]),setPlan(PlanRestant).

*/
%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
%   RECHERCHE EN A GREEDY
%-----------------------------------------------------------------------------
/*

trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, EtatJeu = [_,M,_,_,_,_], trouverPlan_A_Greedy(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_], setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    EtatJeu = [_,M,_,_,_,_], trouverPlan_A_Greedy(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_],[ProchaineAction|PlanRestant]),setPlan(PlanRestant).

*/
%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
%   RECHERCHE EN A*
%-----------------------------------------------------------------------------
/*

trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, EtatJeu = [_,M,_,_,_,_], trouverPlan_A_Star(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_], setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    EtatJeu = [_,M,_,_,_,_], trouverPlan_A_Star(EtatJeu,[_,_,_,_,[[_,'Feim',_,_,M]|_],_],[ProchaineAction|PlanRestant]),setPlan(PlanRestant).

*/
%-----------------------------------------------------------------------------
%   [N,M,C,R,P,B] [_,_,_,_,_,_]
%     – n : le nombre de joueurs, un entier
%     – m : le nombre de blocs, un entier
%     – c : le nombre de colonnes de la surface de jeu, un entier
%     – r : le nombre de rangées de la surface de jeu, un entier
%     – p : les états des joueurs, une liste
%             [id,nom,x,y,b] [_,_,_,_,_]
%                 - id est le numéro du joueur (un entier)
%                 - nom est le nom du joueur (celui fourni par le prédicat de nom)
%                 - x est l’index de la colonne de sa position (un entier)
%                 - y est l’index de la rangée de sa position (un entier)
%                 - b est le numéro du bloc qu’il possède (un entier). 0 ==> Pas de bloc
%     – b : les états des blocs libres, une liste
%             [id,x,y] [_,_,_]
%                 - id est le numéro etla valeur du bloc (un entier)
%                 - x est l’index de la colonne de sa position (un entier)
%                 - y est l’index de la rangée de sa position (un entier)
%-----------------------------------------------------------------------------
% Construction de graph
/*

p11:trouverPlan_Profondeur([4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],[4,3,4,4,[[2,'Feim',3,3,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],P).

p11:trouverPlan_Profondeur([4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],[4,3,4,4,[[2,'Feim',3,3,0]|_],_],P).
*/
%-----------------------------------------------------------------------------


%
% PROFONDEUR
%

trouverPlan_Profondeur(EI,EF,P) :-
  explore_Profondeur([[EI,[]]],[],EF,P).

explore_Profondeur(Liste,_,EF,P) :-
  member([EF,P],Liste).

explore_Profondeur([[E,_]|Z],EX,EF,P) :-
  E \= EF,
  not(member([EF,_],Z)),
  member(E,EX),
  explore_Profondeur(Z,EX,EF,P).

explore_Profondeur([[E,Way]|Z],EX,EF,P) :-
  E \= EF,
  not(member([EF,_],Z)),
  not(member(E,EX)),
  actionsPossibles(E,A),
  append([E],EX,EX2),
  stuffIt_Profondeur([E,Way],EF,A,EX2,L),
  append(L,Z,Adventure),

  %write_ln(Adventure),
  explore_Profondeur(Adventure,EX2,EF,P).

stuffIt_Profondeur(_,_,[],_,[]).

stuffIt_Profondeur([E,Way],EF,[A|_],_,[[EF,NWay]]) :-
  etatSuccesseur(E,A,EF),
  append(Way,[A],NWay).

stuffIt_Profondeur([E,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  member(ES,EX),
  stuffIt_Profondeur([E,Way],EF,Z,EX,L).

stuffIt_Profondeur([E,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  not(member(ES,EX)),
  stuffIt_Profondeur([E,Way],EF,Z,EX,L1),
  append(Way,[A],NWay),
  append([[ES,NWay]],L1,L).

%
% Largeur
%
/*
p11:trouverPlan_Largeur([4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],[4,3,4,4,[[2,'Feim',3,3,0]|_],_],P).
*/
trouverPlan_Largeur(EI,EF,P) :-
  explore_Largeur([[EI,[]]],[],EF,P).

explore_Largeur(Liste,_,EF,P) :-
  member([EF,P],Liste).

explore_Largeur([[E,_]|Z],EX,EF,P) :-
  E \= EF,
  not(member([EF,_],Z)),
  member(E,EX),
  explore_Largeur(Z,EX,EF,P).

explore_Largeur([[E,Way]|Z],EX,EF,P) :-
  E \= EF,
  not(member([EF,_],Z)),
  not(member(E,EX)),
  actionsPossibles(E,A),
  stuffIt_Largeur([E,Way],EF,A,EX,L),
  append(Z,L,Adventure),
  append([E],EX,EX2),
  explore_Largeur(Adventure,EX2,EF,P).

stuffIt_Largeur(_,_,[],_,[]).

stuffIt_Largeur([E,Way],EF,[A|_],_,[[EF,NWay]]) :-
  etatSuccesseur(E,A,EF),
  append(Way,[A],NWay).

stuffIt_Largeur([E,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  member(ES,EX),
  stuffIt_Largeur([E,Way],EF,Z,EX,L).

stuffIt_Largeur([E,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  not(member(ES,EX)),
  stuffIt_Largeur([E,Way],EF,Z,EX,L1),
  append(Way,[A],NWay),
  append([[ES,NWay]],L1,L).

%
% A Greedy
%
/*
p11:trouverPlan_A_Greedy([4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],[4,3,4,4,[[2,'Feim',3,3,0]|_],_],P).
*/
/*
p11:h([1,1,4,4,[[1,'Feim',0,2,0]],[[1,3,0]]],H).
*/
find_THE_Block([_,M,_,_,_,B],X,Y) :-
  look_blocks(M,B,X,Y).
find_THE_Block([_,M,_,_,P,_],X,Y) :-
  look_players(M,P,X,Y).

look_blocks(M,[[M,X,Y]|_],X,Y).
look_blocks(M,[[ID,_,_]|Z],X,Y) :-
  M \= ID,
  look_blocks(M,Z,X,Y).

look_players(M,[[_,_,X,Y,M]|_],X,Y).
look_players(M,[[_,_,_,_,BJ]|Z],X,Y) :-
  M \= BJ,
  look_players(M,Z,X,Y).

h(EtatJeu,H) :-
  %EtatJeu = [_,B,_,_,_,_],
  find_THE_Block(EtatJeu,X,Y),
  myStatus(EtatJeu,[_,_,MyX,MyY,_]),
  H is sqrt( (X-MyX)*(X-MyX) + (Y-MyY)*(Y-MyY) ).
  %H is (B-MyB)*sqrt( (X-MyX)*(X-MyX) + (Y-MyY)*(Y-MyY) ).

/*
calc_H([ [X,Way] ],H,[X,Way]) :- h(X,H).
calc_H([ [X,Way]|Z ],H,[E,Way]) :- h(X,H), write_ln(1:Z), calc_H(Z,H2,_), H <= H2, write_ln(H <= H2).
calc_H([ [X,_]|Z ],H,[E,Way]) :- h(X,H1),write_ln(2:Z), calc_H(Z,H,[E,Way]), write_ln(3:H), H < H1, write_ln(H < H1).
*/


min([X],X) :- !, true.
min([[X,HX,WX]|Xs], [X,HX,WX]):- min(Xs, [_,HY,_]), HY >= HX.
min([[_,HX,_]|Xs], [Y,HY,WY]):- min(Xs, [Y,HY,WY]), HX >  HY.

trouverPlan_A_Greedy(EI,EF,P) :-
  h(EI,H),explore_A_Greedy([[EI,H,[]]],[],EF,P).


explore_A_Greedy(Liste,_,EF,P) :-
  member([EF,_,P],Liste).

explore_A_Greedy(L,EX,EF,P) :-
  min(L,[E,H,Way]),
  E \= EF,
  not(member([EF,_,_],L)),
  member(E,EX),
  delete_in_set([E,H,Way],L,Adventure),
  explore_A_Greedy(Adventure,EX,EF,P).

explore_A_Greedy(L,EX,EF,P) :-
  min(L,[E,H,Way]),
  E \= EF,
  not(member([EF,_,_],L)),
  actionsPossibles(E,A),
  delete_in_set([E,H,Way],L,L1),
  stuffIt_A_Greedy([E,H,Way],EF,A,EX,L2),
  append(L1,L2,Adventure),
  append([E],EX,EX2),
  explore_A_Greedy(Adventure,EX2,EF,P).

stuffIt_A_Greedy(_,_,[],_,[]).

stuffIt_A_Greedy([E,_,Way],EF,[A|_],_,[[EF,H,NWay]]) :-
  etatSuccesseur(E,A,EF),
  append(Way,[A],NWay),
  h(EF,H).

stuffIt_A_Greedy([E,H,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  member(ES,EX),
  stuffIt_A_Greedy([E,H,Way],EF,Z,EX,L).

stuffIt_A_Greedy([E,H,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  not(member(ES,EX)),
  stuffIt_A_Greedy([E,H,Way],EF,Z,EX,L1),
  append(Way,[A],NWay),
  h(ES,H2),
  append([[ES,H2,NWay]],L1,L).


%
% A*
%
len([],0).
len([_|Z],L) :- len(Z,N), L is N+1.

trouverPlan_A_Star(EI,EF,P) :-
  h(EI,H),explore_A_Star([[EI,H,[]]],[],EF,P).


explore_A_Star(Liste,_,EF,P) :-
  member([EF,_,P],Liste).

explore_A_Star(L,EX,EF,P) :-
  min(L,[E,H,Way]),
  E \= EF,
  not(member([EF,_,_],L)),
  member(E,EX),
  delete_in_set([E,H,Way],L,Adventure),
  explore_A_Star(Adventure,EX,EF,P).

explore_A_Star(L,EX,EF,P) :-
  min(L,[E,H,Way]),
  E \= EF,
  not(member([EF,_,_],L)),
  actionsPossibles(E,A),
  delete_in_set([E,H,Way],L,L1),
  stuffIt_A_Star([E,H,Way],EF,A,EX,L2),
  append(L1,L2,Adventure),
  append([E],EX,EX2),
  explore_A_Star(Adventure,EX2,EF,P).

stuffIt_A_Star(_,_,[],_,[]).

stuffIt_A_Star([E,_,Way],EF,[A|_],_,[[EF,H,NWay]]) :-
  etatSuccesseur(E,A,EF),
  append(Way,[A],NWay),
  h(EF,H1),
  len(NWay,Len),
  H is H1 + Len.

stuffIt_A_Star([E,H,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  member(ES,EX),
  stuffIt_A_Star([E,H,Way],EF,Z,EX,L).

stuffIt_A_Star([E,H,Way],EF,[A|Z],EX,L) :-
  etatSuccesseur(E,A,ES),
  ES \= EF,
  not(member(ES,EX)),
  stuffIt_A_Star([E,H,Way],EF,Z,EX,L1),
  append(Way,[A],NWay),
  h(ES,H3),
  len(NWay,Len),
  H2 is H3 + Len,
  append([[ES,H2,NWay]],L1,L).

%-----------------------------------------------------------------------------
% Lecture de l'etat
% [4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]],[4,3,4,4,[[2,'Feim',3,1,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]
%-----------------------------------------------------------------------------

% myStatus(+Etat,-Statut)
myStatus([[ID,Nom,X,Y,B]|_],[ID,Nom,X,Y,B]) :- p11_nom(N), N = Nom.
myStatus([[_,Nom,_,_,_]|Z],L) :- p11_nom(N), N \= Nom, myStatus(Z,L).
myStatus([_,_,_,_,[[ID,Nom,X,Y,B]|_],_],[ID,Nom,X,Y,B]) :- p11_nom(N), N = Nom.
myStatus([_,_,_,_,[[_,Nom,_,_,_]|Z],_],L) :- p11_nom(N), N \= Nom, myStatus(Z,L).

empty([_,_,C,R,P,B],X,Y) :- X>=0, Y>=0, R-1>=X, C-1>=Y, empty_check_P(P,X,Y), empty_check_B(B,X,Y).

empty_check_B([],_,_).
empty_check_B([[_,TX,TY]|Z],TX,Y) :- Y \= TY, empty_check_B(Z,TX,Y).
empty_check_B([[_,TX,TY]|Z],X,TY) :- X \= TX, empty_check_B(Z,X,TY).
empty_check_B([[_,TX,TY]|Z],X,Y) :- X \= TX, Y \= TY, empty_check_B(Z,X,Y).

empty_check_P([],_,_).
empty_check_P([[_,_,TX,TY,_]|Z],TX,Y) :- Y \= TY, empty_check_P(Z,TX,Y).
empty_check_P([[_,_,TX,TY,_]|Z],X,TY) :- X \= TX, empty_check_P(Z,X,TY).
empty_check_P([[_,_,TX,TY,_]|Z],X,Y) :- X \= TX, Y \= TY, empty_check_P(Z,X,Y).

get_player([[ID,Nom,X,Y,B]|_],X,Y,[ID,Nom,X,Y,B]).
get_player([[_,_,_,_,_]|Z],X,Y,R) :- get_player(Z,X,Y,R).

get_block([[ID,X,Y]|_],X,Y,[ID,X,Y]).
get_block([[_,_,_]|Z],X,Y,R) :- get_block(Z,X,Y,R).

%-----------------------------------------------------------------------------
% Actions
%     E = [4,3,4,4,[[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]
%     P = [[2,'Feim',0,2,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]]
%     B = [[1,1,3],[3,3,2],[2,0,1]]
%
%   [4,3,4,4,[[2,'Feim',3,3,0],[3,'Zouf',1,0,0],[1,'Ares',3,0,0],[4,'Buddy',2,2,0]],[[1,1,3],[3,3,2],[2,0,1]]]
%-----------------------------------------------------------------------------
aP(E,R) :-
  actionsPossibles(E,R),write_ln(R).

actionsPossibles(Etat,R) :-
  findall(move(X),move(X,Etat),Moves),
  findall(take(X),take(X,Etat),Takes),
  findall(drop(X),drop(X,Etat),Drops),
  findall(steal(X),steal(X,Etat,_),Steals),
  append(Moves,Takes,L1),
  append(L1,Drops,L2),
  append(L2,Steals,R).

eS(X,A,Y) :-
  etatSuccesseur(X,A,Y), write_ln(Y).

etatSuccesseur([N,M,C,R,P,B],move(X),[N,M,C,R,PF,B]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  adjust(X,PosX,PosY,X1,Y1),
  add_in_set([ID,Nom,X1,Y1,BJ],P1,PF).

etatSuccesseur([N,M,C,R,P,B],take(X),[N,M,C,R,PF,BF]) :-
  myStatus(P,[ID,Nom,PosX,PosY,0]),
  adjust(X,PosX,PosY,X1,Y1),
  get_block(B,X1,Y1,[IDB,X1,Y2]),
  delete_in_set([ID,Nom,PosX,PosY,0],P,P1),
  add_in_set([ID,Nom,PosX,PosY,IDB],P1,PF),
  delete_in_set([IDB,X1,Y2],B,BF).

etatSuccesseur([N,M,C,R,P,B],take(X),[N,M,C,R,PF,BF]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  BJ \= 0,
  adjust(X,PosX,PosY,X1,Y1),
  get_block(B,X1,Y1,[IDB,X1,Y2]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  add_in_set([ID,Nom,PosX,PosY,IDB],P1,PF),
  delete_in_set([IDB,X1,Y2],B,B1),
  add_in_set([BJ,X1,Y2],B1,BF).

etatSuccesseur([N,M,C,R,P,B],drop(X),[N,M,C,R,PF,BF]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  adjust(X,PosX,PosY,X1,Y1),
  add_in_set([ID,Nom,PosX,PosY,0],P1,PF),
  add_in_set(B,[[BJ,X1,Y1]],BF).

etatSuccesseur([N,M,C,R,P,B],steal(X),[N,M,C,R,PF,B]) :-
  myStatus(P,[IDJ,NomJ,PosX,PosY,BJ]),
  adjust(X,PosX,PosY,X1,Y1),
  get_player(P,X1,Y1,[IDE,NomE,X1,Y2,BE]),
  delete_in_set([IDJ,NomJ,PosX,PosY,BJ],P,P1),
  delete_in_set([IDE,NomE,X1,Y2,BE],P1,P2),
  add_in_set([IDE,NomE,X1,Y2,BJ],P2,P3),
  add_in_set([IDJ,NomJ,PosX,PosY,BE],P3,PF).

etatSuccesseur(E,pass(),E).


adjust(1,X1,Y1,X1,Y2) :- Y2 is Y1+1.
adjust(2,X1,Y1,X2,Y1) :- X2 is X1+1.
adjust(3,X1,Y1,X1,Y2) :- Y2 is Y1-1.
adjust(4,X1,Y1,X2,Y1) :- X2 is X1-1.
adjust(5,X1,Y1,X2,Y2) :- X2 is X1+1, Y2 is Y1+1.
adjust(6,X1,Y1,X2,Y2) :- X2 is X1+1, Y2 is Y1-1.
adjust(7,X1,Y1,X2,Y2) :- X2 is X1-1, Y2 is Y1-1.
adjust(8,X1,Y1,X2,Y2) :- X2 is X1-1, Y2 is Y1+1.



move(1,Etat) :- myStatus(Etat,[_,_,X,Y,_]),Y2 is Y+1, empty(Etat,X,Y2).
move(2,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X+1, empty(Etat,X2,Y).
move(3,Etat) :- myStatus(Etat,[_,_,X,Y,_]),Y2 is Y-1, empty(Etat,X,Y2).
move(4,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X-1, empty(Etat,X2,Y).
move(5,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X+1, Y2 is Y+1, empty(Etat,X2,Y2).
move(6,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X+1, Y2 is Y-1, empty(Etat,X2,Y2).
move(7,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X-1, Y2 is Y-1, empty(Etat,X2,Y2).
move(8,Etat) :- myStatus(Etat,[_,_,X,Y,_]),X2 is X-1, Y2 is Y+1, empty(Etat,X2,Y2).

take(1,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),Y2 is Y+1, not(empty_check_B(B,X,Y2)).
take(2,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X+1, not(empty_check_B(B,X2,Y)).
take(3,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),Y2 is Y-1, not(empty_check_B(B,X,Y2)).
take(4,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X-1, not(empty_check_B(B,X2,Y)).
take(5,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X+1, Y2 is Y+1, not(empty_check_B(B,X2,Y2)).
take(6,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X+1, Y2 is Y-1, not(empty_check_B(B,X2,Y2)).
take(7,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X-1, Y2 is Y-1, not(empty_check_B(B,X2,Y2)).
take(8,[_,_,_,_,P,B]) :- myStatus(P,[_,_,X,Y,_]),X2 is X-1, Y2 is Y+1, not(empty_check_B(B,X2,Y2)).

drop(1,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, Y2 is Y+1, empty(Etat,X,Y2).
drop(2,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X+1, empty(Etat,X2,Y).
drop(3,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, Y2 is Y-1, empty(Etat,X,Y2).
drop(4,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X-1, empty(Etat,X2,Y).
drop(5,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X+1, Y2 is Y+1, empty(Etat,X2,Y2).
drop(6,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X+1, Y2 is Y-1, empty(Etat,X2,Y2).
drop(7,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X-1, Y2 is Y-1, empty(Etat,X2,Y2).
drop(8,Etat) :- myStatus(Etat,[_,_,X,Y,B]), B \= 0, X2 is X-1, Y2 is Y+1, empty(Etat,X2,Y2).

steal(1,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), Y2 is Y+1, get_player(P,X,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(2,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X+1, get_player(P,X2,Y,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(3,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), Y2 is Y-1, get_player(P,X,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(4,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X-1, get_player(P,X2,Y,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(5,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X+1, Y2 is Y+1, get_player(P,X2,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(6,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X+1, Y2 is Y-1, get_player(P,X2,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(7,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X-1, Y2 is Y-1, get_player(P,X2,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).
steal(8,[_,_,_,_,P,_],Victory) :- myStatus(P,[_,_,X,Y,B]), X2 is X-1, Y2 is Y+1, get_player(P,X2,Y2,[_,_,_,_,BJ]), BJ \= 0, calc_prob(B,BJ,Victory).

calc_prob(0,_,25).
calc_prob(B1,B2,100*B1/(B1+B2)) :- B1 \= 0.

pass().

%-----------------------------------------------------------------------------
% SET FUNCTIONS
%-----------------------------------------------------------------------------

delete_in_set(_, [], []) :- !.
delete_in_set(E, [E|T], T) :- !.
delete_in_set(E, [H|T], [H|Tnew]) :- delete_in_set(E,T,Tnew).

add_in_set(E, S, S) :- member(E,S), !.
add_in_set(E, S, [E|S]).

/*
    LAB
      [30,2,8,8,[[1,'Inconnu1',4,3,0],[2,'Inconnu2',1,0,0],[3,'Inconnu3',2,4,0],[4,'Inconnu4',3,0,0],[5,'Inconnu5',4,0,0],[6,'Inconnu6',5,0,0],[7,'Inconnu7',6,0,0],[8,'Inconnu8',7,0,0],[9,'Inconnu9',1,5,0],[10,'Inconnu10',1,1,0],[11,'Feim',0,0,0],[12,'Inconnu12',6,6,0],[13,'Inconnu13',6,5,0],[14,'Inconnu14',3,4,0],[15,'Inconnu15',6,1,0],[16,'Inconnu16',4,4,0],[17,'Inconnu17',4,5,0],[18,'Inconnu18',1,2,0],[19,'Inconnu19',2,2,0],[20,'Inconnu20',3,2,0],[21,'Inconnu21',4,2,0],[22,'Inconnu22',5,4,0],[23,'Inconnu23',6,3,0],[24,'Inconnu24',7,2,0],[25,'Inconnu25',4,6,0],[26,'Inconnu26',1,4,0],[27,'Inconnu27',2,5,0],[28,'Inconnu28',2,6,0],[29,'Inconnu29',3,7,0],[30,'Inconnu30',6,4,0]],[[1,0,4],[2,2,0]]].

      [30,1,8,8,[[1,'Inconnu1',4,3,0],[2,'Inconnu2',1,0,0],[3,'Inconnu3',2,4,0],[4,'Inconnu4',3,0,0],[5,'Inconnu5',4,0,0],[6,'Inconnu6',5,0,0],[7,'Inconnu7',6,0,0],[8,'Inconnu8',7,0,0],[9,'Inconnu9',1,5,0],[10,'Inconnu10',1,1,0],[11,'Feim',0,0,0],[12,'Inconnu12',6,6,0],[13,'Inconnu13',6,5,0],[14,'Inconnu14',3,4,0],[15,'Inconnu15',6,1,0],[16,'Inconnu16',4,4,0],[17,'Inconnu17',4,5,0],[18,'Inconnu18',1,2,0],[19,'Inconnu19',2,2,0],[20,'Inconnu20',3,2,0],[21,'Inconnu21',4,2,0],[22,'Inconnu22',5,4,0],[23,'Inconnu23',6,3,0],[24,'Inconnu24',7,2,0],[25,'Inconnu25',4,6,0],[26,'Inconnu26',1,4,0],[27,'Inconnu27',2,5,0],[28,'Inconnu28',2,6,0],[29,'Inconnu29',3,7,0],[30,'Inconnu30',6,4,0]],[[1,2,0]]]
    PDF
       [4,3,4,4,[[2,'Feim',0,2,0],[1,'Inconnu1',2,2,0],[3,'Inconnu3',1,0,0],[4,'Inconnu4',3,0,0]],[[1,0,1],[2,1,3],[3,3,2]]]
    Tester
      [5,1,5,5,[[1,'Inconnu1',1,2,0],[2,'Inconnu2',0,2,0],[3,'Feim',0,0,0],[4,'Inconnu4',2,2,0],[5,'Inconnu5',3,2,0]],[[1,0,4]]]
*/
