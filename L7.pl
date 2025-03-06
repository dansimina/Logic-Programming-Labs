%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 7 EXEMPLE		%%%%%%
%%%%%% 					Trees				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%EXEMPLE DE ARBORI
tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 
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


% ternary_preorder(T, List):- % *IMPLEMENTAȚI AICI*

% ternary_inorder(T, List):- % *IMPLEMENTAȚI AICI*

% ternary_postorder(T, List):- % *IMPLEMENTAȚI AICI*


%--------------------------------------------------
% 2. Scrieți un predicat pretty_print_ternary/1 care face afișarea unui arbore ternar.
% ?- ternary_tree(T), pretty_print_ternary(T).


% pretty_print_ternary(T):- % *IMPLEMENTAȚI AICI*



%--------------------------------------------------
% 3. Scrieți un predicat care calculează înălțimea unui arbore ternar.
% ?- ternary_tree(T), ternary_height(T, H).
% H=3, T= ... ;
% false.

% ternary_height(T, H):- % *IMPLEMENTAȚI AICI*




%--------------------------------------------------
% 4. Scrieți un predicat care colectează într-o listă toate cheile din frunzele arborelui binar de căutare.
% ?- tree1(T), leaf_list(T, R).
% R=[2,5,7], T= ... ;
% false.

% leaf_list(T, List):- % *IMPLEMENTAȚI AICI*




%--------------------------------------------------
% 5. Scrieți un predicat care colectează într-o listă toate cheile interne (nonfrunze) a unui arbore binar de căutare.
% ?- tree1(T), internal_list(T, R).
% R = [4, 6, 9], T = ... ;
% false.

% internal_list(T, List):- % *IMPLEMENTAȚI AICI*




%--------------------------------------------------
% 6. Scrieți un predicat care colectează într-o listă toate nodurile de la aceeași adâncime (inversa înălțimii) din arborele binar.
% ?- tree1(T), same_depth(T, 2, R).
% R = [4, 9], T = ... ;
% false.

% same_depth(T, K, List):- % *IMPLEMENTAȚI AICI*



%--------------------------------------------------
% 7. Scrieți un predicat care calculează diametrul unui arbore binar.
% 𝑑𝑖𝑎𝑚(𝑇) = max {𝑑𝑖𝑎𝑚(𝑇. 𝑙𝑒𝑓𝑡), 𝑑𝑖𝑎𝑚(𝑇. 𝑟𝑖𝑔ℎ𝑡), ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑙𝑒𝑓𝑡) + ℎ𝑒𝑖𝑔ℎ𝑡(𝑇. 𝑟𝑖𝑔ℎ𝑡) + 1}
% ?- tree1(T), diam(T, D).
% D = 5, T = ... ;
% false.


% diam(T, D):- % *IMPLEMENTAȚI AICI*


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


% symmetric(T):- % *IMPLEMENTAȚI AICI*



%--------------------------------------------------
% 9. Rescrieți predicatul delete_key folosind nodul succesor (arbori binari de căutare).
% ?- tree1(T), delete_key(5, T, T1), delete_key_succ(5, T, T2).
% T1 = T2, T2 = t(6,t(4,t(2,nil,nil),nil),t(9,t(7,nil,nil),nil)),
% T = t(6,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,t(7,nil,nil),nil)).
% ?- tree1(T), delete_key(6, T, T1), delete_key_succ(6, T, T2).
% T = t(6,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,t(7,nil,nil),nil)),
% T1 = t(5,t(4,t(2,nil,nil),nil),t(9,t(7,nil,nil),nil)),
% T2 = t(7,t(4,t(2,nil,nil),t(5,nil,nil)),t(9,nil,nil))


% delete_key_succ(K, T, T2):- % *IMPLEMENTAȚI AICI*

