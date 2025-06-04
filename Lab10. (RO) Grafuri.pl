%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 10 EXEMPLE		%%%%%%
%%%%%% 					Graphs 				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------------------------------------------------
% The GRAPH REPRESENTATIONS %
%--------------------------------------------------
% A1B2
node(a). 
node(b). %etc

edge1(a,b). 
edge1(b,a).
edge1(b,c). 
edge1(c,b). %etc

is_edge(X,Y):- edge(X,Y); edge(Y,X).


% A2B2
neighbor1(a, [b, d]).
neighbor1(b, [a, c, d]).
neighbor1(c, [b, d]). %etc.



%--------------------------------------------------
% Predicatul de conversie între reprezentări de grafuri %
%--------------------------------------------------
% declarăm predicatul dinamic pentru a putea folosi retract
:-dynamic neighbor/2.
% predicatul neighbor este considerat a fiind static deoarece este introdus
% în fișier, doar prin adăugarea declarației de dinamicitate ne este permis
% să folosim operația de retract asupra lui


% un exemplu de graf – prima componentă conexă a grafului
neighbor(a, [b, d]).
neighbor(b, [a, c, d]).
neighbor(c, [b, d]).
%etc.

neighb_to_edge:-
    %extrage un predicat neighbor 
    retract(neighbor(Node,List)),!, 
    % și apoi îl procesează
    process(Node,List),
    neighb_to_edge.
neighb_to_edge. % dacă nu mai sunt predicate neighbor/2, ne oprim

% procesarea presupune adăugarea de predicate edge și node 
% pentru un predicat neighbor, prima dată adăugăm muchiile 
% până când lista devine vidă iar abia apoi predicatele de tip node
process(Node, [H|T]):- assertz(gen_edge(Node, H)), process(Node, T).
process(Node, []):- assertz(gen_node(Node)).



% Testați următoarele întrebări:
% ?- neighb_to_edge.
% true;
% false.

% Încercați:
% ?- retractall(gen_edge(_,_)), neighb_to_edge, listing(gen_edge/2).


% Varianta 2, folosind failure-driven loops
neighb_to_edge_v2:-
    neighbor(Node,List), % access the fact
    process(Node,List),
    fail.
neighb_to_edge_v2.



% Variant 3, recursivitate fara retract
:-dynamic seen/1. 

neighb_to_edge_v3:-
    neighbor(Node,List), 
    not(seen(Node)),!,
    assert(seen(Node)),
    process(Node,List),
    neighb_to_edge_v3.
neighb_to_edge_v3. 


%--------------------------------------------------
% Predicatul PATH %
%--------------------------------------------------
edge(a,c).
edge(b,c).
edge(c,d).
edge(d,e).
edge(c,e).

% path(Source, Target, Path)
% drumul parțial începe cu nodul sursă – acesta este un wrapper
path(X, Y, Path):-path(X, Y, [X], Path). 

% când sursa (primul argument) este egal cu destinația (al doilea argument),
% atunci știm că drumul a ajuns la final 
% și putem unifica drumul parțial cu cel final
path(Y, Y, PPath, PPath).			
path(X, Y, PPath, FPath):-
    edge(X, Z), 				% căutăm o muchie
    not(member(Z, PPath)), 		% care nu a mai fost parcursă
    path(Z, Y, [Z|PPath], FPath).	          % și o adăugăm în rezultatul parțial


	
% Urmărește execuția la:
% ?- path(a,c,R).




%--------------------------------------------------
% Predicatul RESTRICTED PATH %
%--------------------------------------------------
% restricted_path(Source, Target, RestrictionsList, Path)
restricted_path(X,Y,LR,P):- 
    path(X,Y,P), 
    reverse(P,PR), 
    check_restrictions(LR, PR).

% predicate that verifies the restrictions
check_restrictions([],_):- !.
check_restrictions([H|TR], [H|TP]):- !, check_restrictions(TR,TP).
check_restrictions(LR, [_|TP]):-check_restrictions(LR,TP).



% Urmărește execuția la:
% ?- check_restrictions([2,3], [1,2,3,4]).
% ?- check_restrictions([1,3], [1,2,3,4]).
% ?- check_restrictions([1,3], [1,2]).
% ?- restricted_path(a, c, [a,c,d], R).


%--------------------------------------------------
% Predicatul OPTIMAL PATH %
%--------------------------------------------------

edge_o(a,c).
edge_o(b,c).
edge_o(c,d).
edge_o(d,e).
edge_o(c,e).
edge_o(a,b).
edge_o(b,e).

:- dynamic sol_part/2.

% optimal_path(Source, Target, Path)
optimal_path(X,Y,Path):-
    asserta(sol_part([], 100)), 	% 100 = distanța maximă inițială
    path(X, Y, [X], Path, 1).		
optimal_path(_,_,Path):- 
    retract(sol_part(Path,_)).

% path(Source, Target, PartialPath, FinalPath, PathLength)
% când ținta este egală cu sursa, salvăm soluția curentă
path(Y,Y,Path,Path,LPath):-	
    % scoatem ultima soluție	
    retract(sol_part(_,_)),!, 	
    % salvăm soluția curentă	
    asserta(sol_part(Path,LPath)),	
    % căutăm o altă soluție
    fail.						
path(X,Y,PPath,FPath,LPath):-
    edge(X,Z),
    not(member(Z,PPath)),
    % calculăm distanța parțială
    LPath1 is LPath+1,	
    % extragem distanța de la soluția precedentă		
    sol_part(_,Lopt),
    % dacă distanța curentă nu depășește distanța precedentă 
    LPath1<Lopt,	
    % mergem mai departe				
    path(Z,Y,[Z|PPath],FPath,LPath1).



% Urmărește execuția la:
% ?- optimal_path(a,c,R).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% 1. Continuă implementarea la ciclul Hamiltonian folosind predicatul hamilton/3.

% ?- hamilton(5, a, P).
% P = [a, e, d, c, b, a]

edge_ex1(a,b).
edge_ex1(b,c).
edge_ex1(a,c).
edge_ex1(c,d).
edge_ex1(b,d).
edge_ex1(d,e).
edge_ex1(e,a).


% hamilton(NumOfNodes, Source, Path)
hamilton(NN, X, Path):- 
    NN1 is NN-1, 
    hamilton_path(NN1, X, X, [X], Path).

is_edge_ex1(X, Y):- edge_ex1(X, Y); edge_ex1(Y, X).

hamilton_path(0, X, Y, Path, [Y|Path]):-
    is_edge_ex1(X, Y).
hamilton_path(NN, X, Y, PPath, Path):-
    is_edge_ex1(X, Z),
    not(member(Z, PPath)),
    NN1 is NN - 1,
    hamilton_path(NN1, Z, Y, [Z|PPath], Path).
    



%--------------------------------------------------
% 2. Scrieți predicatul euler/3 care poate să găsească drumuri Euleriane într-un graf dat de la un nod dat. 
% ?- euler(5, a, R).
% R = [[b, a], [e, b], [d, e], [c, d], [a, c]];
% R = [[c, a], [d, c], [e, d], [b, e], [a, b]]

edge_ex2(a,b).
edge_ex2(b,e).
edge_ex2(c,a).
edge_ex2(d,c).
edge_ex2(e,d).

:- dynamic seen/2.

euler(NE, X, Path):- 
    euler_path(NE, X, Path).

is_edge_ex2(X, Y):- edge_ex2(X, Y); edge_ex2(Y, X).

visited2(X, Y):-
    assert(seen(X, Y)),
    assert(seen(Y, X)).
visited2(X, Y):-
    retract(seen(X, Y)),
    retract(seen(Y, X)), !,
    fail.

euler_path(0, _, []).

euler_path(NE, X, [[Z, X]|Path]):-
    is_edge_ex2(X, Z),
    not(seen(X, Z)),
    visited2(X, Z),
    NE1 is NE - 1,
    euler_path(NE1, Z, Path).

%--------------------------------------------------
% 3. Scrie predicatul cycle(X,R) care găsește un ciclu ce pornește din nodul X dintrun graf G 
% (folosind reprezentarea edge/2) și pune rezultatul în R. Predicatul trebuie să returneze toate 
% ciclurile prin backtracking.
% ?- cycle(a, R).
% R = [a,d,b,a] ;
% R = [a,e,c,a] ;
% false

edge_ex3(a,b).
edge_ex3(a,c).
edge_ex3(c,e).
edge_ex3(e,a).
edge_ex3(b,d).
edge_ex3(d,a).

:- dynamic seen/2.

cycle(X, Path):- 
    cycle(X, X, [X], Path).

cycle(X, Y, PPath, [Y|PPath]):- 
    edge_ex3(X, Y).

cycle(X, Y, PPath, Path):-
    edge_ex3(X, Z),
    not(member(Z, PPath)),
    cycle(Z, Y, [Z|PPath], Path).



%--------------------------------------------------
% 4. Scrieți predicatul cycle(X,R) din exercițiul anterior folosind reprezentarea neighbour/2.
% ?- cycle_neighb(a, R).
% R = [a,d,b,a] ;
% R = [a,e,c,a] ;
% false

neighb_ex4(a, [b,c]).
neighb_ex4(b, [d]).
neighb_ex4(c, [e]).
neighb_ex4(d, [a]).
neighb_ex4(e, [a]).

:- dynamic seen/2.

cycle_neighb(X,Path):- 
    cycle_neighb(X, X, [X], Path).

cycle_neighb(X, Y, PPath, [Y|PPath]):-
    neighb_ex4(X, List),
    member(Y, List).

cycle_neighb(X, Y, PPath, Path):-
    neighb_ex4(X, List),
    member(Z, List),
    not(member(Z, PPath)),
    cycle_neighb(Z, Y, [Z|PPath], Path).
	




%--------------------------------------------------
% 5. Scrieți un predicat care convertește din A1B2 (edge-clause) în A2B2 (neighbor list-clause).
% ?- retractall(gen_neighb_list(_,_)), edge_to_neighb, listing(gen_neighb_list/2).
% true

edge_ex5(a,b).
edge_ex5(a,c).
edge_ex5(b,d).


:- dynamic gen_neighb_list/2.

add_neighb(X, Y):-
    retract(gen_neighb_list(X, List)),
    assert(gen_neighb_list(X, [Y|List])).

make_node(X):-
    gen_neighb_list(X, _), !.
make_node(X):-
    assert(gen_neighb_list(X, [])).

edge_to_neighb:- 
    edge_ex5(X, Y),
    make_node(X),
    make_node(Y),
    add_neighb(X, Y),
    fail.

edge_to_neighb.




%--------------------------------------------------
% 6. Predicatul restricted_path/4 găsește un drum între nodul sursă și cel
% destinație și verifică dacă drumul găsit conține nodurile din lista de restricții.
% Acest predicat folosește recursivitate înainte, ordinea nodurilor trebuie
% inversată în ambele liste – lista de drum și de restricții. Motivați de ce această
% strategie nu este eficientă (urmăriți ce se întâmplă). Scrieți un predicat mai
% eficient care caută un drum restricționat între nodul sursă și cel destinație.
% ? - restricted_path_efficient(a, e, [c,d], P2).
% P = [a, b, c, d, e];
% P = [a, c, d, e];
% false

edge_ex6(a,b).
edge_ex6(b,c).
edge_ex6(a,c).
edge_ex6(c,d).
edge_ex6(b,d).
edge_ex6(d,e).
edge_ex6(e,a).

valid6(X, [H|T], LR):-
    X = H, LR = T, !;
    not(member(X, T)), LR = [H|T].

visited6(X):-
    assert(seen(X)).
visited6(X):-
    retract(seen(X)),
    fail.

restricted_path_efficient(X, X, _, [X]):- !.

restricted_path_efficient(X,Y,LR,[X|Path]):- 
    not(seen(X)),
    valid6(X, LR, NLR),
    visited6(X),
    edge_ex6(X, Z),
    restricted_path_efficient(Z, Y, NLR, Path).




%--------------------------------------------------
% 7. Rescrie optimal_path/3 astfel încât să funcționeze pe grafuri ponderate:
% atașați o pondere pentru fiecare muchie din graf și calculați drumul de cost
% minim dintr-un nod sursă la un nod destinație.
% ?- optimal_weighted_path(a, e, P).
% P = [e, b, a]

edge_ex7(a,c,7).
edge_ex7(a,b,10).
edge_ex7(c,d,3).
edge_ex7(b,e,1).
edge_ex7(d,e,2).


optimal_weighted_path(X, Y, P):- 
	assert(sol_part(_, 100)),
    weight_path(X, Y, 0, [X], P).
optimal_weighted_path(_, _, P):-
    retract(sol_part(P, _)).

weight_path(Y, Y, PWeight, P, P):-
    retract(sol_part(_, _)), !,
    asserta(sol_part(P, PWeight)),
    fail.

weight_path(X, Y, PWeight, PPath, P):-
    edge_ex7(X, Z, W),
    not(member(Z, PPath)),
    sol_part(_, Best),
    PWeight1 is PWeight + W,
    PWeight1 < Best,
    weight_path(Z, Y, PWeight1, [Z|PPath], P).
    
    
    

%--------------------------------------------------
% 8. Scrie o serie de predicate care să rezolve problema Lupul-Capra-Varza: “un
% fermier trebuie să mute în siguranță, de pe malul nordic pe malul sudic, un
% lup, o capră și o varză. În barcă încap maxim doi și unul dintre ei trebuie să
% fie fermierul. Dacă fermierul nu este pe același mal cu lupul și capra, lupul
% va mânca capra. Același lucru se întâmplă și cu varza și capra, capra va mânca
% varza.”
% Sugestii:
% ● Puteți alege să encodați spațiul de stări ca instanțe a unei
%	configurări ale celor 4 obiecte (Fermir, Lup, Capră, Varză):
%	reprezentate ca o listă cu poziția celor 4 obiecte [Fermier, Lup,
%	Capră, Varză] sau ca o structură complexă (ex. F-W-G-C, sau
% 	state(F,W,G,C)).
% ● starea inițială este [n,n,n,n] (toți sunt în nord) și starea finală
%	este [s,s,s,s] (toți au ajuns în sud); pentru reprezentarea folosind
%	liste a stărilor (ex. dacă Fermierul trece lupul -> [s,s,n,n], această
%	stare nu ar trebui să fie validă deoarece capra mănâncă varza).
% ● la fiecare trecere Fermierul își schimba poziția (din „n” în „s” sau
%	invers) și cel mult încă un participant (Lupul, Capra, Varza).
% ● problema poate fi văzută ca o problema de căutare a drumului
%	într-un graf.

%F-W-G-C
not_valid([s, n, n, s]).
not_valid([s, s, n, n]).
not_valid([n, n, s, s]).
not_valid([n, s, s, n]).

valid(S):- not(not_valid(S)).

swap_n([], []).
swap_n([n|T], [s|T]).
swap_n([H|T], [H|R]):-
    swap_n(T, R).

swap_s([], []).
swap_s([s|T], [n|T]).
swap_s([H|T], [H|R]):-
    swap_s(T, R).
    
solve(R):- solve([n, n, n, n], [[n, n, n, n]], R).

solve([s, s, s, s], P, P).

solve([s|T], PP, P):-
    swap_s(T, TR),
    valid([n|TR]),
    not(member([n|TR], PP)),
    solve([n|TR], [[n|TR]|PP], P).

solve([n|T], PP, P):-
	swap_n(T, TR),
    valid([s|TR]),
    not(member([s|TR], PP)),
    solve([s|TR], [[s|TR]|PP], P).
