%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 7 EXEMPLE		%%%%%%
%%%%%% 					Trees				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%EXEMPLE DE ARBORI
%tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
%tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 
% …
% prin această întrebare, variabila T se unifică cu arborele din faptul tree1/1
% ?- tree1(T), operatie_pe_arbore(T, …).



%--------------------------------------------------
% Predicatele de TRAVERSARE %
%--------------------------------------------------

% subarbore stâng, cheie și subarbore drept (ordinea din append)
inorder(t(K,L,R), List):-
	inorder(L,LL), 
	inorder(R, LR),
	append(LL, [K|LR],List).
inorder(nil, []). 

% cheie, subarbore stâng și subarbore drept (ordinea din append)
preorder(t(K,L,R), List):-
	preorder(L,LL), 
	preorder(R, LR),
	append([K|LL], LR, List).
preorder(nil, []).

% subarbore stâng, subarbore drept și apoi cheia (ordinea din append-uri)
postorder(t(K,L,R), List):-
	postorder(L,LL), 
	postorder(R, LR),
	append(LL, LR, R1), 
	append(R1, [K], List).
postorder(nil, []). 


% Urmărește execuția la:
% ?- tree1(T), inorder(T, L).
% ?- tree1(T), preorder(T, L).
% ?- tree1(T), postorder(T, L).




%--------------------------------------------------
% Predicatul PRETTY PRINT %
%--------------------------------------------------

% wrapper
pretty_print(T):- pretty_print(T, 0).

% predicatul care printează arborele
pretty_print(nil, _).
pretty_print(t(K,L,R), D):- 
	D1 is D+1,
	pretty_print(L, D1),
	print_key(K, D),
	pretty_print(R, D1).

% predicat care afișează cheia K la D tab-uri față de marginea din stânga
% și inserează o linie nouă (prin nl)
print_key(K, D):-D>0, !, D1 is D-1, tab(8), print_key(K, D1).
print_key(K, _):-write(K), nl.
% write('\n') echivalent cu nl/0

% Urmărește execuția la:
% ?- tree2(T), pretty_print(T).



%--------------------------------------------------
% Predicatul SEARCH %
%--------------------------------------------------
search_key(Key, t(Key, _, _)):-!.
search_key(Key, t(K, L, _)):-Key<K, !, search_key(Key, L).
search_key(Key, t(_, _, R)):-search_key(Key, R).


% Urmărește execuția la:
% ?- tree1(T), search_key(5,T).
% ?- tree1(T), search_key(8,T).



%--------------------------------------------------
% Predicatul INSERT %
%--------------------------------------------------
insert_key(Key, nil, t(Key, nil, nil)). % inserează cheia în arbore
insert_key(Key, t(Key, L, R), t(Key, L, R)):- !. % cheia există deja
insert_key(Key, t(K,L,R), t(K,NL,R)):- Key<K,!,insert_key(Key,L,NL).
insert_key(Key, t(K,L,R), t(K,L,NR)):- insert_key(Key, R, NR).


% Urmărește execuția la:
% ?- tree1(T),pretty_print(T),insert_key(8,T,T1),pretty_print(T1).
% ?- tree1(T),pretty_print(T),insert_key(5,T,T1),pretty_print(T1).
% ?- insert_key(7, nil, T1), insert_key(12, T1, T2), insert_key(6, T2, T3), insert_key(9, T3, T4), insert_key(3, T4, T5), insert_key(8, T5, T6), insert_key(3, T6, T7), pretty_print(T7).




%--------------------------------------------------
% Predicatul DELETE %
%--------------------------------------------------
delete_key(Key, t(Key, L, nil), L):- !.
delete_key(Key, t(Key, nil, R), R):- !.
delete_key(Key, t(Key, L, R), t(Pred,NL,R)):- !, get_pred(L,Pred,NL).
delete_key(Key, t(K,L,R), t(K,NL,R)):- Key<K, !, delete_key(Key,L,NL).
delete_key(Key, t(K,L,R), t(K,L,NR)):- delete_key(Key,R,NR).

% caută nodul predecesor
get_pred(t(Pred, L, nil), Pred, L):-!.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):- get_pred(R, Pred, NR).


% Urmărește execuția la:
% ?- tree1(T), pretty_print(T), delete_key(5, T, T1), pretty_print(T1).
% ?- tree1(T), pretty_print(T), delete_key(9, T, T1), pretty_print(T1).
% ?- tree1(T), pretty_print(T), delete_key(6, T, T1), pretty_print(T1).
% ?- tree1(T), pretty_print(T), insert_key(8, T, T1), pretty_print(T1), delete_key(6, T1, T2), pretty_print(T2), insert_key(6, T2, T3), pretty_print(T3).


%--------------------------------------------------
% Predicatul HEIGHT %
%--------------------------------------------------

% predicatul care calculează maximul dintre două numere date
max(X, Y, X):- X>Y, !.
max(_, Y, Y).

% predicatul care calculează înalțimea unui arbore binar
height(nil, 0).
height(t(_, L, R), H):- 
    height(L, H1),
 	height(R, H2),
 	max(H1, H2, H3),
 	H is H3+1.


% Urmărește execuția la:
% ?- tree1(T), pretty_print(T), height(T, H).
% ?- tree1(T), height(T, H), pretty_print(T), insert_key(8, T, T1), height(T1, H1), pretty_print(T1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arbori:
tree1(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))).
ternary_tree(t(6, t(4, t(2, nil, nil, nil), nil, t(7, nil, nil, nil)), t(5, nil, nil, nil), t(9, t(3, nil, nil, nil), nil, nil))).


%--------------------------------------------------
% 1. Scrieți predicatele care iterează peste elementele unui arbore ternar în:
% 1.1. preorder=Root->Left->Middle->Right
% 1.2. inorder=Left->Root->Middle->Right
% 1.3. postorder=Left->Middle->Right->Root


% ?- ternary_tree(T), ternary_preorder(T, L).
% L = [6, 4, 2, 7, 5, 9, 3], T= ...
% ?- ternary_tree(T), ternary_inorder(T, L).
% L = [2, 4, 7, 6, 5, 3, 9], T= ...
% ?- ternary_tree(T), ternary_postorder(T, L).
% L = [2, 7, 4, 5, 3, 9, 6], T= ... 


ternary_preorder(nil, []).
ternary_preorder(t(Key, Left, Middle, Right), List):-
    ternary_preorder(Left, LL),
    ternary_preorder(Middle, LM),
    ternary_preorder(Right, LR),
    append(LM, LR, LI),
    append([Key|LL], LI, List).

ternary_inorder(nil, []).
ternary_inorder(t(Key, Left, Middle, Right), List):- 
    ternary_inorder(Left, LL),
    ternary_inorder(Middle, LM),
    ternary_inorder(Right, LR),
    append([Key|LM], LR, LI),
    append(LL, LI, List).

ternary_postorder(nil, []).
ternary_postorder(t(Key, Left, Middle, Right), List):-
    ternary_postorder(Left, LL),
    ternary_postorder(Middle, LM),
    ternary_postorder(Right, LR),
    append(LR, [Key], LI1),
    append(LM, LI1, LI2),
    append(LL, LI2, List).




%--------------------------------------------------
% 2. Scrieți un predicat pretty_print_ternary/1 care face afișarea unui arbore ternar.
% ?- ternary_tree(T), pretty_print_ternary(T).


pretty_print_ternary(nil, _).
pretty_print_ternary(t(Key, Left, Middle, Right), D):-
    print_key(Key, D),
    D1 is D + 1,
    pretty_print_ternary(Left, D1),
    pretty_print_ternary(Middle, D1),
    pretty_print_ternary(Right, D1).
pretty_print_ternary(T):- pretty_print_ternary(T, 0).
    



%--------------------------------------------------
% 3. Scrieți un predicat care calculează înălțimea unui arbore ternar.
% ?- ternary_tree(T), ternary_height(T, H).
% H=3, T= ... ;
% false.


max3(X, Y, Z, R):- max(X, Y, R1), max(R1, Z, R).

ternary_height(nil, 0).
ternary_height(t(_, Left, Middle, Right), H):- 
	ternary_height(Left, H1),
    ternary_height(Middle, H2),
    ternary_height(Right, H3),
    max3(H1, H2, H3, HM),
    H is HM + 1.



%--------------------------------------------------
% 4. Scrieți un predicat care colectează într-o listă toate cheile din frunzele arborelui binar de căutare.
% ?- tree1(T), leaf_list(T, R).
% R=[2,5,7], T= ... ;
% false.

leaf_list(nil, []).
leaf_list(t(Key, nil, nil), [Key]):- !.
leaf_list(t(_, Left, Right), List):- 
    leaf_list(Left, LL),
    leaf_list(Right, LR),
    append(LL, LR, List).




%--------------------------------------------------
% 5. Scrieți un predicat care colectează într-o listă toate cheile interne (nonfrunze) a unui arbore binar de căutare.
% ?- tree1(T), internal_list(T, R).
% R = [4, 6, 9], T = ... ;
% false.

internal_list(nil, []).
internal_list(t(_, nil, nil), []):- !.
internal_list(t(Key, Left, Right), List):- 
    internal_list(Left, LL),
    internal_list(Right, LR),
    append(LL, [Key|LR], List).




%--------------------------------------------------
% 6. Scrieți un predicat care colectează într-o listă toate nodurile de la aceeași adâncime (inversa înălțimii) din arborele binar.
% ?- tree1(T), same_depth(T, 2, R).
% R = [4, 9], T = ... ;
% false.


same_depth(nil, _, _, []).
same_depth(t(Key, _, _), K, K, [Key]):- !.
same_depth(t(_, Left, Right), K, D, List):-
    D1 is D + 1,
    same_depth(Left, K, D1, LL),
    same_depth(Right, K, D1, LR),
    append(LL, LR, List).
same_depth(T, K, List):- same_depth(T, K, 1, List).




%--------------------------------------------------
% 7. Scrieți un predicat care calculează diametrul unui arbore binar.
% 𝑑𝑖𝑎𝑚(𝑇) = max {𝑑𝑖𝑎𝑚(𝑇. 𝑙𝑒𝑓𝑡), 𝑑𝑖𝑎𝑚(𝑇. 𝑟𝑖𝑔ℎ𝑡), ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑙𝑒𝑓𝑡) + ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑟𝑖𝑔ℎ𝑡) + 1}
% ?- tree1(T), diam(T, D).
% D = 5, T = ... ;
% false.


diam(nil, 0, 0).
diam(t(_, Left, Right), D, H):-
    diam(Left, DL, HL),
    diam(Right, DR, HR),
    max(HL, HR, HI),
    H is HI + 1,
    DI is HL + HR + 1,
    max3(DL, DR, DI, D).
    
diam(T, D):- diam(T, D, _).

    
    

%--------------------------------------------------
% 8. Scrieți un predicat care verifică dacă un arbore binar este simetric. Un
% arbore binar este simetric dacă subarborele stâng este imaginea în
% oglindă a subarborelui drept. Ne interesează structura arborelui nu și
% valorile (cheile) din noduri. 
% ?- tree1(T), symmetric(T).
% false.
% ?- tree1(T), delete_key(2, T, T1), symmetric(T1).
% T = t(6,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,t(7,nil,nil),nil)),
% T1 = t(6,t(4,nil,t(5,nil,nil)),t(9,t(7,nil,nil),nil));
% false.


symmetric(nil, nil).
symmetric(t(_, Left1, Right1), t(_, Left2, Right2)):-
    symmetric(Left1, Right2),
    symmetric(Right1, Left2).

symmetric(t(_, Left, Right)):- symmetric(Left, Right).



%--------------------------------------------------
% 9. Rescrieți predicatul delete_key folosind nodul succesor (arbori binari de căutare).
% ?- tree1(T), delete_key(5, T, T1), delete_key_succ(5, T, T2).
% T1 = T2, T2 = t(6,t(4,t(2,nil,nil),nil),t(9,t(7,nil,nil),nil)),
% T = t(6,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,t(7,nil,nil),nil)).
% ?- tree1(T), delete_key(6, T, T1), delete_key_succ(6, T, T2).
% T = t(6,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,t(7,nil,nil),nil)),
% T1 = t(5,t(4,t(2,nil,nil),nil),t(9,t(7,nil,nil),nil)),
% T2 = t(7,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,nil,nil))


delete_key_succ(K, t(K, L, nil), L):- !.
delete_key_succ(K, t(K, nil, R), R):- !.
delete_key_succ(K, t(K, L, R), t(Succ, L, NR)):- !, get_succ(R, Succ, NR).
delete_key_succ(K, t(Key, L, R), t(Key, NL, R)):- K < Key, !, delete_key_succ(K, L, NL).
delete_key_succ(K, t(Key, L, R), t(Key, L, NR)):- delete_key_succ(K, R, NR).

get_succ(t(Succ, nil, R), Succ, R):- !.
get_succ(t(Key, L, R), Succ, t(Key, NL, R)):-
    get_succ(L, Succ, NL).

tree_to_deep_list(nil, []).
tree_to_deep_list(t(Key, Left, Right), [LList, Key, RList]):-
    tree_to_deep_list(Left, LList),
    tree_to_deep_list(Right, RList).

% nod impar cu un singur copil
sum_odd(nil, 0).
sum_odd(t(Key, Left, nil), R):- 
    Left \= nil,
    1 is Key mod 2, !,
    sum_odd(Left, LR),
    sum_odd(Right, RR),
    R is LR + RR + Key.
sum_odd(t(Key, nil, Right), R):- 
    Right \= nil,
    1 is Key mod 2, !,
    sum_odd(Left, LR),
    sum_odd(Right, RR),
    R is LR + RR + Key.
sum_odd(t(_, Left, Right), R):- 
    sum_odd(Left, LR),
    sum_odd(Right, RR),
    R is LR + RR.


sum_desc(nil, nil, 0).
sum_desc(t(Key, Left, Right), t(Sum, NL, NR), Sum):-
    sum_desc(Left, NL, SL),
    sum_desc(Right, NR, SR),
    Sum is SL + SR + Key.
sum_desc(T, TR):- sum_desc(T, TR, _).
    


