%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 8 EXEMPLE		%%%%%%
%%%%%% Incomplete Structures (Lists & Trees)%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
%--------------------------------------------------
% LISTE %
%--------------------------------------------------
%--------------------------------------------------

%--------------------------------------------------
% Predicatul MEMBER %
%--------------------------------------------------
% trebuie testat explicit faptul ca am ajuns la sfârșitul listei
% ceea ce înseamnă că nu am găsit elementul căutat, așa că apelăm fail
member_il(_, L):-var(L), !, fail.
% celelalte 2 clauze sunt la fel ca în trecut
member_il(X, [X|_]):-!.
member_il(X, [_|T]):-member_il(X, T).


% % Urmărește execuția la:
% ?- L = [1, 2, 3|_], member_il(3, L).
% ?- L = [1, 2, 3|_], member_il(4, L).
% ?- L = [1, 2, 3|_], member_il(X, L).


%--------------------------------------------------
% Predicatul INSERT %
%--------------------------------------------------

% am ajuns la finalul listei atunci putem adăuga elementul
insert_il1(X, L):-var(L), !, L=[X|_].
insert_il1(X, [X|_]):-!. % elementul există deja
insert_il1(X, [_|T]):- insert_il1(X, T). %traversăm lista să ajungem la final


% *SIMPLIFICAT
insert_il2(X, [X|_]):-!.
insert_il2(X, [_|T]):- insert_il2(X, T).

% Urmărește execuția la:
% ?- L = [1, 2, 3|_], insert_il2(3, L).
% ?- L = [1, 2, 3|_], insert_il2(4, L).
% ?- L = [1, 2, 3|_], insert_il2(X, L).




%--------------------------------------------------
% Predicatul DELETE %
%--------------------------------------------------
delete_il(_, L, L):-var(L), !. % am ajuns la finalul listei
delete_il(X, [X|T], T):- !. % găsim, ștergem prima apariție și ne oprim
delete_il(X, [H|T], [H|R]):- delete_il(X, T, R). % traversăm, căutam elementul

% Urmărește execuția la:
% ?- L = [1, 2, 3|_], delete_il(2, L, R).
% ?- L = [1, 2, 3|_], delete_il(4, L, R).
% ?- L = [1, 2, 3|_], delete_il(X, L, R).





%--------------------------------------------------
%--------------------------------------------------
% ARBORI %
%--------------------------------------------------
%--------------------------------------------------

%--------------------------------------------------
% Predicatul SEARCH %
%--------------------------------------------------
search_it(_, T):- var(T), !, fail.
search_it(Key, t(Key, _, _)):- !.
search_it(Key, t(K, L, _)):- Key<K, !, search_it(Key, L).
search_it(Key, t(_, _, R)):- search_it(Key, R).

% Urmărește execuția la:
% ?- T=t(7, t(5, t(3,_,_), t(6,_,_)), t(11,_,_)), search_it(6, T).
% ?- T=t(7, t(5, t(3,_,_), _), t(11,_,_)), search_it(9, T).


%--------------------------------------------------
% Predicatul INSERT %
%--------------------------------------------------
% inserează sau verifică dacă elementul există deja în arbore
insert_it(Key, t(Key, _, _)):-!.
insert_it(Key, t(K, L, _)):-Key<K, !, insert_it(Key, L).
insert_it(Key, t(_, _, R)):-insert_it(Key, R).

% Urmărește execuția la:
% ?- T=t(7, t(5, t(3,_,_), t(6,_,_)), t(11,_,_)), insert_it(6, T).
% ?- T=t(7, t(5, t(3,_,_), _), t(11,_,_)), insert_it(9, T).



%--------------------------------------------------
% Predicatul DELETE %
%--------------------------------------------------
delete_it(_, T, T):- var(T), !. % elementul se găsește în arbore
delete_it(Key, t(Key, L, R), L):- var(R), !.
delete_it(Key, t(Key, L, R), R):- var(L), !.
delete_it(Key, t(Key, L, R), t(Pred,NL,R)):- !, get_pred(L,Pred,NL).
delete_it(Key, t(K,L,R), t(K,NL,R)):- Key<K, !, delete_it(Key,L,NL).
delete_it(Key, t(K,L,R), t(K,L,NR)):- delete_it(Key,R,NR).

% caută nodul predecesor
get_pred(t(Pred, L, R), Pred, L):- var(R), !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):- get_pred(R, Pred, NR).

% Urmărește execuția la:
% ?- T=t(7, t(5, t(3,_,_), t(6,_,_)), t(11,_,_)), delete_it(6, T, R).
% ?- T=t(7, t(5, t(3,_,_), _), t(11,_,_)), delete_it(9, T, R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arbori:
incomplete_tree(t(7, t(5, t(3, _, _), t(6, _, _)), t(11, _, _))).
complete_tree(t(7, t(5, t(3, nil, nil), t(6, nil, nil)), t(11, nil, nil))).


% Scrieți un predicat care:

%--------------------------------------------------
% 1. Convertește o listă incompletă într-o listă completă și viceversa.
% ?- convertIL2CL([1,2,3|_], R).
% R = [1, 2, 3].
% ?- convertCL2IL([1,2,3], R).
% R = [1, 2, 3|_].

convertIL2CL(L, []):- var(L), !.
convertIL2CL([H|T], [H|R]):-
    convertIL2CL(T, R).

convertCL2IL([], _).
convertCL2IL([H|T], [H|R]):- 
    convertCL2IL(T, R).




%--------------------------------------------------
% 2. Concatenează 2 liste incomplete (rezultatul este tot o listă incompletă –
% De câte argumente este nevoie, am putea renunța la unul?).
% ?- append_il([1,2|_], [3,4|_], R).
% R = [1, 2, 3, 4|_].


append_il(L1, L2, L2):- var(L1), !.
append_il([H|T], L2, [H|R]):- 
	append_il(T, L2, R).

append_test(L1, L2):- var(L1), !, L1 = L2.
append_test([_|T], L2):-
    append_test(T, L2).
% L1=[1,2|_], L2=[3,4|_], append_test(L1, L2).




%--------------------------------------------------
% 3. Inversează o listă incompletă (rezultatul este tot o listă incompletă).
% Implementați în ambele tipuri de recursivitate.
% ?- reverse_il_fwd([1,2,3|_], R).
% R = [3, 2, 1|_].
% ?- reverse_il_bwd([1,2,3|_], R).
% R = [3, 2, 1|_].


reverse_il_fwd(L, R, R):- var(L), !.
reverse_il_fwd([H|T], Acc, R):-
    reverse_il_fwd(T, [H|Acc], R).
reverse_il_fwd(L, R):- reverse_il_fwd(L, _, R).

reverse_il_bwd(L, L):- var(L), !.
reverse_il_bwd([H|T], R):- 
    reverse_il_bwd(T, R),
    append_test(R, [H|_]).



%--------------------------------------------------
% 4. Aplatizează o listă adâncă incompletă (rezultatul este o listă simplă incompletă).
% ?- flat_il([[1|_], 2, [3, [4, 5|_]|_]|_], R).
% R = [1, 2, 3, 4, 5|_] ;
% false.


flat_il(L, L):- var(L), !.
flat_il([H|T], [H|R]):- 
    atomic(H), !, 
    flat_il(T, R).
flat_il([H|T], R):-
    flat_il(H, R),
    flat_il(T, RT),
    append_test(R, RT).
    



%--------------------------------------------------
% 5. Convertește un arbore incomplet într-un arbore complet și viceversa.
% ?- incomplete_tree(T), convertIT2CT(T, R).
% R = t(7,t(5,t(3,nil,nil),t(6,nil,nil)),t(11,nil,nil))
% ?- complete_tree(T), convertCT2IT(T, R).
% R = t(7, t(5, t(3, _, _), t(6, _, _)), t(11, _, _))

convertIT2CT(T, nil):- var(T), !.
convertIT2CT(t(Key, Left, Right), t(Key, NL, NR)):-
    convertIT2CT(Left, NL),
    convertIT2CT(Right, NR).

convertCT2IT(nil, _).
convertCT2IT(t(Key, Left, Right), t(Key, NL, NR)):- 
    convertCT2IT(Left, NL),
    convertCT2IT(Right, NR).



%--------------------------------------------------
% 6. Traversează un arbore incomplet în pre-ordine (cheile se adaugă într-o listă incompletă).
% ?- incomplete_tree(T), preorder_it(T, R).
% R = [7, 5, 3, 6, 11|_]


preorder_it(L, _):- var(L), !.
preorder_it(t(Key, Left, Right), R):- 
    preorder_it(Left, RL),
    preorder_it(Right, RR),
    append_il([Key|RL], RR, R).




%--------------------------------------------------
% 7. Calculează înălțimea unui arbore binar incomplet.
% ?- incomplete_tree(T), height_it(T, R).
% R=3

max(A, B, A):- A > B, !.
max(_, B, B).

height_it(T, 0):- var(T), !.
height_it(t(_, Left, Right), R):- 
	height_it(Left, RL),
    height_it(Right, RR),
    max(RL, RR, Int),
    R is Int + 1.
    



%--------------------------------------------------
% 8. Calculează diametrul unui arbore binar incomplet.
% 𝑑𝑖𝑎𝑚(𝑇) = max {𝑑𝑖𝑎𝑚(𝑇. 𝑙𝑒𝑓𝑡), 𝑑𝑖𝑎𝑚(𝑇. 𝑟𝑖𝑔ℎ𝑡), ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑙𝑒𝑓𝑡) + ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑟𝑖𝑔ℎ𝑡) + 1}
% ?- incomplete_tree(T), diam_it(T, R).
% R=4


diam_it(T, 0, 0):- var(T), !.
diam_it(t(_, Left, Right), H, R):-
    diam_it(Left, HL, RL),
    diam_it(Right, HR, RR),
    max(HL, HR, Hint),
    H is Hint + 1,
    D is HL + HR + 1,
    max(RL, RR, Rint),
    max(D, Rint, R).
diam_it(T, R):- diam_it(T, _, R).


%--------------------------------------------------
% 9. Determină dacă o listă incompletă este o sub-listă într-o altă listă incompletă.
% ?- subl_il([1, 1, 2|_], [1, 2, 3, 1, 1, 3, 1, 1, 1, 2, 3,4|_]).
% true
% ?- subl_il([1, 1, 2|_], [1, 2, 3, 1, 1, 3, 1, 1, 1, 3, 2, 4|_]).
% false

append3_il(_,_,_,R):-var(R), !, fail.
append3_il(L1, L2, R, R):- var(L1), var(L2), !.
append3_il(L1, [H2|T2], L3, [H2|R]):- 
    var(L1), !,
    append3_il(L1, T2, L3, R).
append3_il([H1|T1], L2, L3, [H1|R]):- 
    append3_il(T1, L2, L3, R).

subl_il(SL, L):- append3_il(_, SL, _, L).



%--------------------------------------------------
% 10. Scrieți predicatul append_il/2 de concatenare a două liste incomplete folosind doar două argumente (fără argument pentru rezultat)








