%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 5 EXEMPLE		%%%%%%
%%%%%% 			Sorting Methods				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------------------------------------------------
% Predicatul PERMUTATION SORT %
%--------------------------------------------------
perm_sort(L,R):-perm(L, R), is_ordered(R), !.

perm(L, [H|R]):- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([H1, H2|T]):- H1 =< H2, is_ordered([H2|T]).
is_ordered([_]). % if only one element remains, the list is already ordered


% Urmărește execuția la:
% ?- append(A, [H|T], [1, 2, 3]), append(A, T, R).
% ?- perm([1, 2, 3], L).
% ?- is_ordered([1, 2, 4, 4, 5]).
% ?- perm_sort([1, 4, 2, 3, 5], R).



%--------------------------------------------------
% Predicatul SELECTION SORT %
%--------------------------------------------------
sel_sort(L, [M|R]):- min1(L, M), delete1(M, L, L1), sel_sort(L1, R).
sel_sort([], []).

delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).
delete1(_, [], []).

min1([H|T], M) :- min1(T, M), M<H, !.
min1([H|_], H).


% Urmărește execuția la:
% ?- sel_sort([3, 2, 4, 1], R).
% ?- sel_sort([3, 1, 5, 2, 4, 3], R).



%--------------------------------------------------
% Predicatul INSERTION SORT %
%--------------------------------------------------
ins_sort([H|T], R):- ins_sort(T, R1), insert_ord(H, R1, R).
ins_sort([], []).

insert_ord(X, [H|T], [H|R]):-X>H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]). 

% Urmărește execuția la:
% ?- insert_ord(3, [], R).
% ?- insert_ord(3, [1, 2, 4, 5], R).
% ?- insert_ord(3, [1, 3, 3, 4], R).
% ?- ins_sort([3, 2, 4, 1], R).




%--------------------------------------------------
% Predicatul BUBBLE SORT %
%--------------------------------------------------
bubble_sort(L, R):-one_pass(L, R1, F), nonvar(F), !, bubble_sort(R1, R).
bubble_sort(L, L).

one_pass([H1, H2|T], [H2|R], F):- H1>H2, !, F = 1, one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F):-one_pass(T, R, F).
one_pass([], [] ,_).


% Urmărește execuția la:
% ?- one_pass([1, 2, 3, 4], R, F).
% ?- one_pass([2, 3, 1, 4], R, F).
% ?- bubble_sort([1, 2, 3, 4], R).
% ?- bubble_sort([2, 3, 1, 4], R).




%--------------------------------------------------
% Predicatul QUICK SORT %
%--------------------------------------------------
quick_sort([H|T], R):-
	partition(H, T, Sm, Lg), 
    quick_sort(Sm, SmS),
	quick_sort(Lg, LgS), 
    append(SmS, [H|LgS], R).
quick_sort([], []).

partition(H, [X|T], [X|Sm], Lg):-X<H, !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]):-partition(H, T, Sm, Lg).
partition(_, [], [], []).


% Urmărește execuția la:
% ?- partition(3, [4, 2, 6, 1, 3], Sm, Lg).
% ?- quick_sort([3, 2, 5, 1, 4, 3], R).
% ?- quick_sort([1, 2, 3, 4], R).


%--------------------------------------------------
% Predicatul MERGE SORT %
%--------------------------------------------------
merge_sort(L, R):- 
	split(L, L1, L2), 
    merge_sort(L1, R1), 
    merge_sort(L2, R2),
	merge(R1, R2, R).
merge_sort([H], [H]).
merge_sort([], []).

split(L, L1, L2):-
	length(L, Len), 
	Len>1, 
	K is Len/2, 
	splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2):- K>0, !, K1 is K-1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]):-H1<H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]):-merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).



% Urmărește execuția la:
% ?- split([2, 5, 1, 6, 8, 3], L1, L2).
% ?- split([2], L1, L2).
% ?- merge([1, 5, 7], [3, 6, 9], R).
% ?- merge([1, 1, 2], [1], R).
% ?- merge([], [3], R).
% ?- merge_sort([4, 2, 6, 1, 5], R).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% 1. Rescrieți predicatul sel_sort/2 astfel încât să selecționeze cea mai mare
% valoare din partea nesortată, prin urmare să sorteze descrescător,
% folosind denumirea sel_sort_max/2. Predicatul max1/2 poate fi creat prin
% modificarea predicatului min1/2 din laboratorul anterior. 
% ?- sel_sort_max([3,4,1,2,5], R).
% R = [5, 4, 3, 2, 1];
% false

max1([H|T], M):- max1(T, M), H < M, !.
max1([H|_], H).

sel_sort_max(L, [M|R]):- max1(L, M), delete1(M, L, R1), sel_sort_max(R1, R).
sel_sort_max([], []).




%--------------------------------------------------
% 2. Rescrieți predicatul ins_sort utilizând recursivitate forward, folosind
% denumirea ins_sort_fwd/2.
% Recomandare: predicatul de insertion sort este format din două
% predicate, ambele folosind o abordare backwards, incercați să le
% modificați pe ambele într-o abordare forwards.
% ?- ins_sort_fwd([3,4,1,2,5], R).
% R = [1, 2, 3, 4, 5];
% false

ins_sort_fwd([H|T], Acc, R):- insert_ord(H, Acc, Acc1), ins_sort_fwd(T, Acc1, R).
ins_sort_fwd([], Acc, Acc).

ins_sort_fwd(L, R):- ins_sort_fwd(L, [], R).




%--------------------------------------------------
% 3. Implementați bubble sort cu un număr fix de treceri prin lista de intrare,
% folosind denumirea bubble_sort_fixed/3.
% ?- bubble_sort_fixed([3,5,4,1,2], 2, R).
% R = [3, 1, 2, 4, 5]

one_pass([H1,H2|T], [H2|R]):- H1 > H2, !, one_pass([H1|T], R).
one_pass([H1,H2|T], [H1|R]):- one_pass([H2|T], R).
one_pass([H], [H]).
one_pass([], []).

bubble_sort_fixed(L, K, R):- K > 0,  K1 is K - 1, one_pass(L, R1), !, bubble_sort_fixed(R1, K1, R).
bubble_sort_fixed(L, _, L).




%--------------------------------------------------
% 4. Scrieți un predicat care să sorteze o listă de caractere ASCII. (Puteți folosi
% o metodă de sortare la alegere).
% Sugestie: folosiți predicatul predefinit char_code/2
% ?- sort_chars([e, t, a, v, f], L).
% L = [a, e, f, t, v] ;
% false

smaller_chars(X, Y):- nonvar(X), nonvar(Y), char_code(X, CX), char_code(Y, CY), CY > CX.

partition_chars(X, [H|T], [H|Sm], Lg):-
	smaller_chars(H, X), !,
    partition_chars(X, T, Sm, Lg).
partition_chars(X, [H|T], Sm, [H|Lg]):-
    partition_chars(X, T, Sm, Lg).
partition_chars(_, [], [], []).

sort_chars([H|T], R):- 
    partition_chars(H, T, Sm, Lg),
    sort_chars(Sm, SmR),
    sort_chars(Lg, LgR),
    append(SmR, [H|LgR], R).
sort_chars([], []).



%--------------------------------------------------
% 5. Scrieți un predicat care să sorteze o lista de sub-liste în funcție de lungimea sub-listelor
% ?- sort_lens([[a, b, c], [f], [2, 3, 1, 2], [], [4, 4]], R).
% R = [[], [f], [4, 4], [a, b, c], [2, 3, 1, 2]] ;
% false

smaller_lens(L1, L2):- nonvar(L1), nonvar(L2), length(L1, N1), length(L2, N2), N2 > N1, !.

partition_lens(X, [H|T], [H|Sm], Lg):- 
    smaller_lens(H, X), !,
    partition_lens(X, T, Sm, Lg).
partition_lens(X, [H|T], Sm, [H|Lg]):-
	partition_lens(X, T, Sm, Lg).
partition_lens(_, [], [], []).

sort_lens([H|T], R):- 
    partition_lens(H, T, Sm, Lg),
	sort_lens(Sm, SmR),
    sort_lens(Lg, LgR),
	append(SmR, [H|LgR], R).
sort_lens([], []).



%--------------------------------------------------
% Opțional. Acest predicat poate fi îngreunat atunci când luăm în considerare
% cazul a două subliste cu lungimi egale, luăm cazul [1,1,1] și [1,1,2],
% predicatul ar trebui să analizeze suplimentar cele două liste în cazul de
% lungimi egale și să compare element cu element.
% ?- sort_lens2([[], [1], [2, 3, 1, 2], [2, 3, 5, 2], [7,6,8], [4, 4]], R).
% R = [[], [1], [4, 4], [7, 6, 8], [2, 3, 1, 2], [2, 3, 5, 2]];
% false

smaller_list2([H1|T1], [H2|T2]):- H2 >= H1, !, smaller_list2(T1, T2).
smaller_list2([], _).

smaller__lens2(L1, L2):- smaller_lens(L1, L2), !.
smaller__lens2(L1, L2):- nonvar(L1), nonvar(L2), smaller_list2(L1, L2). 

partition_lens2(X, [H|T], [H|Sm], Lg):-
    smaller__lens2(H, X), !,
    partition_lens2(X, T, Sm, Lg).
partition_lens2(X, [H|T], Sm, [H|Lg]):-
    partition_lens2(X, T, Sm, Lg).
partition_lens2(_, [], [], []).
    
sort_lens2([H|T], R):- 
    partition_lens2(H, T, Sm, Lg),
    sort_lens2(Sm, SmR),
    sort_lens2(Lg, LgR),
    append(SmR, [H|LgR], R).
sort_lens2([], []).



%--------------------------------------------------
% 6. Rescrieți predicatul perm/2 fără a apela predicatul append/3, folosind
% denumirea perm1/2. Extragerea și ștergerea unui element trebuie realizate altfel.
% ?- perm1([1,2,3], R).
% R = [1, 2, 3];
% R = [1, 3, 2];
% R = [2, 1, 3];
% …

perm1(L, [X|R]):- 
    member(X, L), 
	delete(L, X, L1),
    perm1(L1, R).
perm1([], []).

%7
arrangements(0, _, []).
arrangements(K, L, [X|R]):-
    K > 0,
    K1 is K - 1,
    member(X, L), 
    delete(L, X, L1),
    arrangements(K1, L1, R).

delete2(X, [X|T], T).
delete2(X, [H|T], [H|R]):- delete2(X, T, R).
delete2(_, [], []).

arrangements1(0, _, []).
arrangements1(K, L, [X|R]):-
    K > 0,
    K1 is K - 1,
    delete2(X, L, L1),
    nonvar(X),
    arrangements1(K1, L1, R).
    


