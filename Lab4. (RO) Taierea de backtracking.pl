%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 4 EXEMPLE		%%%%%%
%%%%%% 				The Cut					%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------------------------------------------------
% Predicatul MEMBER %
%--------------------------------------------------
member1(X, [X|_]) :- !.
member1(X, [_|T]) :- member1(X, T).


% Urmărește execuția la:
% ?- member1(X,[1,2,3])
% ?- X=3, member1(X, [3, 2, 4, 3, 1, 3]).
% ?- member1(X, [3, 2, 4, 3, 1, 3]).



%--------------------------------------------------
% Predicatul DELETE %
%--------------------------------------------------
delete1(X, [X|T], T) :- !.
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).
delete1(_, [], []).


% Urmărește execuția la:
% ?- delete1(3,[4,3,2,3,1], R).
% ?- delete1(X, [3, 2, 4, 3, 1, 3], R).



%--------------------------------------------------
% Predicatul LENGTH %
%--------------------------------------------------
% Varianta 1 (recursivitate înapoi)
length1([], 0).
length1([_|T], Len) :- length1(T, Lcoada), Len is 1+Lcoada.


% Varianta 2 (recursivitate înainte -> acumulator = al doilea argument)
length2([], Acc, Len) :- Len=Acc.
length2([_|T], Acc, Len) :- Acc1 is Acc + 1, length2(T, Acc1, Len).

length2(L, R) :- length2(L, 0, R).
% length2/2 = wrapper al predicatului length2/3 care folosește un acumulator


% Urmărește execuția la:
% ?- length1([a, b, c, d], Len).
% ?- length1([1, [2], [3|[4]]], Len).
% ?- length2([a, b, c, d], 0, Res).
% ?- length2([a, b, c, d], Len).
% ?- length2([1, [2], [3|[4]]], Len).
% ?- length2([a, b, c, d], 3, Len).





%--------------------------------------------------
% Predicatul REVERSE %
%--------------------------------------------------
% Varianta 1 (recursivitate înapoi)
reverse1([], []).
reverse1([H|T], R) :- reverse1(T, Rtail), append1(Rtail, [H], R).

append1([], L2, L2).
append1([H|T], L2, [H|CoadaR]) :- append(T, L2, CoadaR).



% Varianta 2 (recursivitate înainte –> acumulator = al doilea argument)
reverse2([], Acc, R) :- Acc=R.
reverse2([H|T], Acc, R) :- Acc1=[H|Acc], reverse2(T, Acc1, R).

reverse2(L, R) :- reverse2(L, [], R).
% reverse2/2 = wrapper a predicatului reverse2/3 care
% folosește un acumulator

% În contrast cu acumulatoarele de până acum, aici avem operații cu liste,
% astfel va fi instanțiată cu o listă vidă



% Urmărește execuția la:
% ?- reverse1([a, b, c, d], R).
% ?- reverse1([1, [2], [3|[4]]], R).
% ?- reverse2([a, b, c, d], R).
% ?- reverse2([1, [2], [3|[4]]], R).
% ?- reverse2([a, b, c, d], [1, 2], R).





%--------------------------------------------------
% Predicatul MIN %
%--------------------------------------------------

% Varianta 1 (recursivitate înainte –> acumulator = al doilea argument)
min1([], Mp, M) :- M=Mp.
min1([H|T], Mp, M) :- H<Mp, !, min1(T, H, M).
min1([_|T], Mp, M) :- min1(T, Mp, M).

min1([H|T], M) :- min1(T, H, M).
% În contrast cu acumulatoarea folosite până acum
% pentru predicatul min1/3,
% acumulatorul (al doilea argument) va fi inițializat cu primul element
% Într-un mod similar, min1/2 este un wrapper.



% Varianta 2 (recursivitate înapoi)
min2([H|T], M) :- min2(T, M), M<H, !.
min2([H|_], H).


% Urmărește execuția la:
% ?- min1([], M).
% ?- min1([3, 2, 6, 1, 4, 1, 5], M).
% ?- min2([], M).
% ?- min2([3, 2, 6, 1, 4, 1, 5], M).





%--------------------------------------------------
% Operații pe Seturi %
%--------------------------------------------------
% Prin folosirea predicatului member și recursivitate, putem verifica fiecare
% element (H) a listei L1 dacă este un element și a L2:
% dacă este -> nu va fi adăugat în rezultatul R
% Altfel, îl adăugăm in R.
union([], L, L).
union([H|T], L2, R) :- member(H, L2), !, union(T, L2, R).
union([H|T], L2, [H|R]) :- union(T, L2, R).


% Urmărește execuția la:
% ?-union([1,2,3], [4,5,6], R).
% ?-union([1,2,5], [2,3], R).
% ?-union(L1,[2,3,4],[1,2,3,4,5]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% 1. Scrieți predicatul intersect(L1,L2,R) care realizează intersecția între două mulțimi.
% Sugestie: Gândiți-vă la cum a fost implementat predicatul union. Rețineți – vom lua doar elementele care apar în ambele liste.
% Remember - take only the elements that appear in both lists.
% ?- intersect([1,2,3], [1,3,4], R).
% R = [1, 3] ;
% false


intersect([H1|T1], L2, [H1|R]):-
    member1(H1, L2), !,
    intersect(T1, L2, R).
intersect([_|T1], L2, R):- 
    intersect(T1, L2, R).
intersect([], _, []).




%--------------------------------------------------
% 2. Scrieți predicatul diff(L1,L2,R) care realizează diferența între două
% mulțimi (elementele care apar în prima mulțime și nu apar în a doua mulțime).
%Sugestie: Gândiți-vă la cum a fost implementat predicatul union și
% intersect. Rețineți – vom lua doar elementele care apar în prima listă și nu și in a doua
% ?- diff([1,2,3], [1,3,4], R).
% R = [2] ;
% false


diff([H1|T1], L2, R):- 
    member1(H1, L2), !,
    diff(T1, L2, R).
diff([H1|T1], L2, [H1|R]):-
    diff(T1, L2, R).
diff([], _, []).




%--------------------------------------------------
% 3. Scrieți predicatele del_min(L,R) și del_max(L,R) care șterg toate aparițiile
% minimului, respectiva ale maximului din lista L.
% ?- del_min([1,3,1,2,1], R).
% R = [3, 2] ;
% false
% ?- del_max([3,1,3,2,3], R).
% R = [1, 2] ;
% false


% del_min(L, R):-  % *IMPLEMENTAȚI AICI*


% del_max(L, R):-  % *IMPLEMENTAȚI AICI*





%--------------------------------------------------
% Exercițiu greu: Încercați să implementați fiecare din aceste predicate folosind o singură traversare a listei (del_min1/2 și del_max1/2). (del_min1/2 and del_max1/2).


add_if_not_min(M, M, L, L):- !.
add_if_not_min(X, _, L, [X|L]).

del_min1([H|T], Mp, M, R):- 
    H >= Mp, !,
    del_min1(T, Mp, M, R1),
    add_if_not_min(H, M, R1, R).
del_min1([H|T], _, M, R):- 
    del_min1(T, H, M, R1),
    add_if_not_min(H, M, R1, R).
del_min1([], M, M, []).
    
del_min1([H|T], R):- del_min1([H|T], H, _, R).


add_if_not_max(M, M, L, L):- !.
add_if_not_max(X, _, L, [X|L]).

del_max1([H|T], Mp, M, R):- 
    H > Mp, !,
    del_max1(T, H, M, R1),
    add_if_not_max(H, M, R1, R).
del_max1([H|T], Mp, M, R):- 
    del_max1(T, Mp, M, R1),
    add_if_not_max(H, M, R1, R).
del_max1([], M, M, []).

del_max1([H|T], R):- del_max1([H|T], H, _, R).





%--------------------------------------------------
% 4. Scrieți un predicat care inversează ordinea elementelor dintr-o listă începând cu al K-lea element.
% ?- reverse_k([1, 2, 3, 4, 5], 2, R).
% R = [1, 2, 5, 4, 3] ;
% false

reverse_k([H|T], K, Acc, Cnt, R):-
    Cnt >= K, !, 
    reverse_k(T, K, [H|Acc], Cnt, R).
reverse_k([H|T], K, Acc, Cnt, [H|R]):- 
    Cnt1 is Cnt + 1,
    reverse_k(T, K, Acc, Cnt1, R).
reverse_k([], _, R, _, R).

reverse_k(L, K, R):- reverse_k(L, K, [], 0, R).




%--------------------------------------------------
% 5. Scrieți un predicat care codifică șirul de elemente folosind algoritmul RLE
% (Run-length encoding). Un șir de elemente consecutive și egale se vor
% înlocui cu perechi [element, număr de apariții].
% pair.
% ?- rle_encode([1, 1, 1, 2, 3, 3, 1, 1], R).
% R = [[1, 3], [2, 1], [3, 2], [1, 2]] ;
% false


rle_encode([H,H|T], Cnt, R):-
	!,
    Cnt1 is Cnt + 1,
    rle_encode([H|T], Cnt1, R).
rle_encode([H1,H2|T], Cnt, [[H1, Cnt1]|R]):- 
    Cnt1 is Cnt + 1,
    rle_encode([H2|T], 0, R).
rle_encode([H], Cnt, [[H, Cnt1]]):- 
    Cnt1 is Cnt + 1.
rle_encode([], _, []).

rle_encode(L, R):- rle_encode(L, 0, R).





%--------------------------------------------------
% Opțional. Puteți modifica acest predicat astfel încât dacă un element este
% singular (nu are valori consecutive egale), să păstram acel element în loc să adăugam o pereche?
% ?- rle_encode2([1, 1, 1, 2, 3, 3, 1, 1], R).
% R = [[1, 3], 2, [3, 2], [1, 2]] ;
% false


add_rle_encode2(X, Cnt, L, [X|L]):- 1 is Cnt, !.
add_rle_encode2(X, Cnt, L, [[X, Cnt]|L]).

rle_encode2([H,H|T], Cnt, R):-
	!,
    Cnt1 is Cnt + 1,
    rle_encode2([H|T], Cnt1, R).
rle_encode2([H1,H2|T], Cnt, R):- 
    Cnt1 is Cnt + 1,
    rle_encode2([H2|T], 0, R1),
    add_rle_encode2(H1, Cnt1, R1, R).
rle_encode2([H], Cnt, R):- 
    Cnt1 is Cnt + 1,
    add_rle_encode2(H, Cnt1, [], R).
rle_encode2([], _, []).

rle_encode2(L, R):- rle_encode2(L, 0, R).




%--------------------------------------------------
% 6. Scrieți un predicat care rotește o listă K poziții la dreapta.
% Sugestie: încercați să implementați rotate_left mai întâi, este mai ușor.
% ?- rotate_right([1, 2, 3, 4, 5, 6], 2, R).
% R = [5, 6, 1, 2, 3, 4] ;
% false


rotate_right(L, K, R):- append(X, Y, L), length(Y, K), append(Y, X, R).




%--------------------------------------------------
% 7. Scrieți un predicat care extrage aleatoriu K element din lista L și le pune în lista rezultat R.
% Sugestie: folosiți predicatul predefinit random_between/3(min_value, max_value, result).
% ?- rnd_select([a, b, c, d, e, f, g, h], 3, R).
% R = [e, d, a] ;
% false

dec_K(X, I, K, K, I):- member(X, I), !.
dec_K(X, I, K, RK, [X|I]):-
    RK is K - 1.

random_k(0, _, _, Acc, Acc).
random_k(K, Min, Max, Acc, R):- 
         random_between(Min, Max, R1),
    	 dec_K(R1, Acc, K, K1, Acc1),
         random_k(K1, Min, Max, Acc1, R), !.

select([H|T], I, Pos, [H|R]):- 
    member(Pos, I), !, 
    Pos1 is Pos - 1,
    select(T, I, Pos1, R).
select([_|T], I, Pos, R):-
    Pos1 is Pos - 1,
    select(T, I, Pos1, R).
select([], _, _, []).

rnd_select(L, K, R):- length(L, Len), random_k(K, 1, Len, [], I), select(L, I, Len, R).
    



%--------------------------------------------------
% 8. Scrieți un predicat care decodifică șirul de elemente folosind algoritmul
% RLE (Run-length encoding). O pereche [element, număr de apariții] va fi
% înlocuită de un șir de elemente consecutive și egale. 
% ?- rle_decode([[1, 3], [2, 1], [3, 2], [1, 2]], R).
% R = [1, 1, 1, 2, 3, 3, 1, 1] ;
% false


rle_decode([[E, C]|T], [E|R]):- 
    C > 1, !,
    C1 is C - 1,
    rle_decode([[E, C1]|T], R).
rle_decode([[E|_]|T], [E|R]):- 
    rle_decode(T, R).
rle_decode([], []).

