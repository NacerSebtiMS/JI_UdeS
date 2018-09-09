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

planInitial([move(1),move(2),move(3),move(4)]).

planRestant([move(1),move(2),move(3),move(4)]).

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
% Calcul de la prochaine action du JI. Ce JI ne fera jamais rien de bon...
%-----------------------------------------------------------------------------
/*
trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction]), !, planInitial(P), setPlan(P).
trouveAction(EtatJeu, ProchaineAction) :-
    getPlan([ProchaineAction|PlanRestant]), setPlan(PlanRestant).
*/

%-----------------------------------------------------------------------------
%   [n,m,c,r,p,b] [_,_,_,_,_,_]
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
%-----------------------------------------------------------------------------
trouverPlan(EF,EF,[]).

trouverPlan(EI,EF,Plan) :-
  creer_profondeur(EI,EF,[EI],Plan).

creer_profondeur(EI,EF,_,[Action]) :-
  actionsPossibles(EI,[Action|_]),
  etatSuccesseur(EI,Action,EF).

creer_profondeur(EI,EF,EXE,Plan) :-
  actionsPossibles(EI,[Action|_]),
  etatSuccesseur(EI,Action,ES),
  not(member(ES,EXE)),
  ES \= EF,
  add_in_set(ES,EXE,E1),
  creer_profondeur(ES,EF,E1,P2),
  add_in_set([Action],P2,Plan).

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

actionsPossibles(Etat,R) :-
  findall(move(X),move(X,Etat),Moves),
  findall(take(X),take(X,Etat),Takes),
  findall(drop(X),drop(X,Etat),Drops),
  findall(steal(X),steal(X,Etat,_),Steals),
  append(Moves,Takes,L1),
  append(L1,Drops,L2),
  append(L2,Steals,R).

etatSuccesseur([N,M,C,R,P,B],move(X),[N,M,C,R,PF,B]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  adjust(X,PosX,PosY,X1,Y1),
  add_in_set([ID,Nom,X1,Y1,BJ],P1,PF).

etatSuccesseur([N,M,C,R,P,B],take(X),[N,M,C,R,PF,BF]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  adjust(X,PosX,PosY,X1,Y1),
  get_block(B,X1,Y1,[IDB,X1,Y2]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  add_in_set([ID,Nom,PosX,PosY,IDB],P1,PF),
  delete_in_set([IDB,X1,Y2],B,BF).

etatSuccesseur([N,M,C,R,P,B],drop(X),[N,M,C,R,PF,BF]) :-
  myStatus(P,[ID,Nom,PosX,PosY,BJ]),
  delete_in_set([ID,Nom,PosX,PosY,BJ],P,P1),
  adjust(X,PosX,PosY,X1,Y1),
  add_in_set([ID,Nom,PosX,PosY,0],P1,PF),
  add_in_set(B,[BJ,X1,Y1],BF).

etatSuccesseur([N,M,C,R,P,B],steal(X),[N,M,C,R,PF,B]) :-
  myStatus(P,[IDJ,NomJ,PosX,PosY,BJ]),
  adjust(X,PosX,PosY,X1,Y1),
  get_player(P,X1,Y1,[IDE,NomE,X1,Y2,BE]),
  delete_in_set([IDJ,NomJ,PosX,PosY,BJ],P,P1),
  delete_in_set([IDE,NomE,X1,Y2,BE],P1,P2),
  add_in_set([IDJ,NomJ,PosX,PosY,BJ],P2,P3),
  add_in_set([IDE,NomE,X1,Y2,BE],P3,PF).

etatSuccesseur(E,pass(),E).


adjust(1,X1,Y1,X1,Y2) :- Y2 is Y1+1.
adjust(2,X1,Y1,X2,Y1) :- X2 is X1+1.
adjust(3,X1,Y1,X1,Y2) :- Y2 is Y1-1.
adjust(4,X1,Y1,X2,Y1) :- X2 is X1-1.
adjust(5,X1,Y1,X2,Y2) :- X2 is X1+1, Y2 is Y1+1.
adjust(6,X1,Y1,X2,Y2) :- X2 is X1+1, Y2 is Y1-1.
adjust(7,X1,Y1,X2,Y2) :- X2 is X1-1, Y2 is Y1-1.
adjust(8,X1,Y1,X2,Y2) :- X2 is X1+1, Y2 is Y1+1.

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
