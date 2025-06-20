%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 11 EXEMPLE		%%%%%%
%%%%%% Algoritmi de traversare a grafurilor %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

edge(1,2).
edge(1,5).
edge(2,3).
edge(2,5).
edge(3,4).
edge(4,5).
edge(4,6).

is_edge(X,Y):- edge(X,Y);edge(Y,X).


%--------------------------------------------------
% Predicatul DFS %
%--------------------------------------------------
:- dynamic nod_vizitat/1.

% dfs(Source, Path)
dfs(X,_) :- df_search(X). % parcurgerea nodurilor
% când parcurgerea se termină, începe colectarea
dfs(_,L) :- !, collect_reverse([], L). % colectarea rezultatelor

% predicatul de traversare
df_search(X):-
    % salvăm X ca nod vizitat
    asserta(nod_vizitat(X)),
    % luăm un prim edge de la X la un Y
    % restul le vom găsi prin backtracking
    is_edge(X,Y),
    % verificăm daca acest Y a fost deja vizitat
    not(nod_vizitat(Y)),
    % dacă nu a fost -  de aceea avem nevoie de negare – 
    % atunci vom continua parcurgerea prin mutarea nodululi curent la Y
    df_search(Y).

% predicatul de colectare - colectarea se face în ordine inversă
collect_reverse(L, P):-
    % scoatem fiecare nod vizitat
    retract(nod_vizitat(X)), !, 
    % îl adăugăm la lista ca primul element
    % astfel vor aparea în ordine inversă
    collect_reverse([X|L], P).
    % unificăm primul si al doilea argument, 
    % rezultatul va fi în al doilea argument
collect_reverse(L,L).



% Urmărește execuția la:
% ?- dfs(1,R).
% R = [1, 2, 3, 4, 5, 6].




%--------------------------------------------------
% Predicatul BFS %
%--------------------------------------------------
:- dynamic nod_vizitat/1.
:- dynamic coada/1. 	% coada reține nodurile care trebuie expandate

% bfs(Source, Path)
bfs(X, _):- % parcurgerea nodurilor
    assertz(nod_vizitat(X)), % adăugăm sursa ca nod vizitat
    assertz(coada(X)), % adăugăm sursa în coadă
    bf_search.
bfs(_,R):- !, collect_reverse([],R). % colectarea rezultatelor

bf_search:-
    retract(coada(X)), % scoatem nodul care trebuie expandat
    expand(X), !, % apelăm predicatul de expansiune
    bf_search. % recursivitate
	
expand(X):-	
    is_edge(X,Y), % găsim un nod Y cu o muchie la X-ul dat
    not(nod_vizitat(Y)), % verificăm daca Y a fost vizitat
    asserta(nod_vizitat(Y)), % adăugăm Y la nodurile vizitate
    assertz(coada(Y)), % adăugam Y în coadă pentru a fi expandat 
    % la un moment dat
    fail. % fail-ul este necesar pentru a găsi un alt Y
expand(_).  


% Urmărește execuția la:
% ?- bfs(1,R).
% R = [1, 2, 5, 3, 4, 6].


%--------------------------------------------------
% Predicatul Best-First Search %
%--------------------------------------------------
pos_vec(start,0,2,[a,d]).
pos_vec(a,2,0,[start,b]).
pos_vec(b,5,0,[a,c, end]).
pos_vec(c,10,0,[b, end]).
pos_vec(d,3,4,[start,e]).
pos_vec(e,7,4,[d]).
pos_vec(end,7,2,[b,c]).

is_target(end).



best([], []):-!.
best([[Target|Rest]|_], [Target|Rest]):- is_target(Target),!.
best([[H|T]|Rest], Best):-
	pos_vec(H,_,_, Neighb),
	expand(Neighb, [H|T], Rest, Exp),
	quick_sort(Exp, SortExp, []),
	best(SortExp, Best).

% Bazat pe calea curentă (al doilea argument), predicatul expand/4 
% caută prin vecinii ultimului nod expandat (primul argument) 
expand([],_,Exp,Exp):- !.
expand([H|T],Path,Rest,Exp):- 
	\+(member(H,Path)), !, expand(T,Path,[[H|Path]|Rest],Exp).
expand([_|T],Path,Rest,Exp):- expand(T,Path,Rest,Exp).

% Predicatul quick_sort/3 utilizează liste diferență
quick_sort([H|T],S,E):-
	partition(H,T,A,B),
	quick_sort(A,S,[H|Y]),
	quick_sort(B,Y,E).
quick_sort([],S,S).

% În acest caz, predicatul partition/4 folosește un predicat auxiliar
% order/2 care definește modul de a partiționa ca fiind 
% bazat pe distanțte
partition(H,[A|X],[A|Y],Z):- order(A,H), !, partition(H,X,Y,Z).
partition(H,[A|X],Y,[A|Z]):- partition(H,X,Y,Z).
partition(_,[],[],[]).

% predicat care calculează distanța între două noduri
dist(Node1,Node2,Dist):-
pos_vec(Node1, X1, Y1, _),
pos_vec(Node2, X2, Y2, _),
	Dist is (X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2).

% predicatul order/2 bazat pe distanțe folosit in partition/4
order([Node1|_],[Node2|_]):- 
is_target(Target),
	dist(Node1,Target,Dist1),
	dist(Node2,Target,Dist2),
	Dist1<Dist2.




% Urmărește execuția la:
% ?- best([[start]], Best).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% 1. Modificați predicatul DFS astfel încât să caute noduri numai până la o anumită adâncime (DLS – Depth-Limited Search). Setați limita de adâncime printr-un predicat, depth_max(2). de exemplu.
% ?- dfs_ex1(a,DFS), dls(a, DLS).
% DFS = [a, b, d, e, g, c, f, h],
% DLS = [a, b, d, c, f].

depth_max(2).

edge_ex1(a,b).
edge_ex1(a,c).
edge_ex1(b,d).
edge_ex1(d,e).
edge_ex1(c,f).
edge_ex1(e,g).
edge_ex1(f,h).

:- dynamic visited_node/1.

is_edge_ex1(X, Y):- edge_ex1(X, Y); edge_ex1(Y, X).
collect_ex1([X|R]):- 
    retract(visited_node(X)), !,
    collect_ex1(R).
        
collect_ex1([]).

dfs_ex1(X, _):- dfs_search(X).
dfs_ex1(_, R):- !, collect_ex1(R).

dfs_search(X):-
    assertz(visited_node(X)),
    is_edge_ex1(X, Y),
    not(visited_node(Y)),
    dfs_search(Y).

dls(X, _):- dls_search(X, 0).
dls(_, R):- !, collect_ex1(R).

dls_search(X, D):-
    assertz(visited_node(X)),
    depth_max(DEPTH_MAX),
    D < DEPTH_MAX,
    D1 is D + 1,
    is_edge_ex1(X, Y),
	not(visited_node(Y)),
    dls_search(Y, D1).
 
 
% 2.	Având următoarea secvență de cod în Prolog care implementează algoritmul BFS fără efecte laterale:
% 2.1.	Modificați astfel încât să nu necesite utilizarea predicatului reverse (spre exemplu, prin folosirea listelor diferență).
% 2.2.	Modificați astfel încât să funcționeze pe reprezentarea de edge în loc de reprezentarea de neighbor.
% ?- bfs1(a, R).
% R = [a, b, c, d, e].

neighbor(a, [b,c]).
neighbor(b, [a,d]).
neighbor(c, [a,e]).
neighbor(d, [b]).
neighbor(e, [c]).


bfs1(X, R) :-
    bfs1([X], [], R).

bfs1([], _, []).
bfs1([X|Q], V, [X|R]):- 
    \+member(X, V),
    neighbor(X, Ns),
    remove_visited(Ns, V, RemNs),
    append(Q, RemNs, NewQ),
    bfs1(NewQ, [X|V], R).

remove_visited([], _, []).
remove_visited([H|T], V, [H|R]):- \+member(H, V), !, remove_visited(T, V, R).
remove_visited([_|T], V, R):- remove_visited(T, V, R).


bfs2(X, SR):-
    bfs2([X|EQ], EQ, EV, EV, SR, []).

bfs2(SQ, EQ, SR, ER, SR, ER):- SQ == EQ, var(EQ), !.
bfs2([X|SQ1], EQ, SV, EV, SR, ER):- 
    \+member2(X, SV, EV),
    neighbor(X, Ns),
    remove_visited2(Ns, SV, EV, EQ, EQ1),
    EV = [X|EV1],
    bfs2(SQ1, EQ1, SV, EV1, SR, ER).

remove_visited2([], _, _, ER, ER).
remove_visited2([H|T], SV, EV, [H|SR], ER):- \+member2(H, SV, EV), !, remove_visited2(T, SV, EV, SR, ER).
remove_visited2([_|T], SV, EV, SR, ER):- remove_visited2(T, SV, EV, SR, ER).

member2(_, S, E):- S == E, var(E), !, fail.
member2(X, [X|_], _):- !.
member2(X, [_|T], E):-
    member2(X, T, E).


edge3(a, b).
edge3(a, c).
edge3(b, d).
edge3(c, e).

is_edge3(X, Y):- edge3(X, Y); edge3(Y, X).

bfs3(X, R):-
    bfs3([X], [], R).

bfs3([], _, []).
bfs3([X|Q], V, [X|R]):- 
    \+member(X, V),
    expand3(X, V, RemNs),
    append(Q, RemNs, NewQ),
    bfs3(NewQ, [X|V], R).

expand3(X, V, [Y|R]):- 
    is_edge3(X, Y),
    \+member(Y, V), !,
    expand3(X, [Y|V], R).
expand3(_, _, []).

