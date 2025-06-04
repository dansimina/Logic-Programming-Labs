%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 		Recapitulare pentru Colocviu	%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------------------------------------------------
% 1	Operații aritmetice %
%--------------------------------------------------

%1.	Calculați cel mai mare divizor comun a două numere. 
%?- cmmdc(15,25,R). 
%R = 5. 

swap_cmmdc(A, B, B, A):- A > B, !.
swap_cmmdc(A, B, A, B).

cmmdc(0, B, B):- !.
cmmdc(A, B, R):- 
	B1 is B - A,
    swap_cmmdc(A, B1, RA, RB),
    cmmdc(RA, RB, R).
    

%2.	Calculați cel mai mic multiplu comun a două numere. 
%?- cmmmc(15,25,R). 
%R = 75. 

cmmmc(_, _, N, N, N):- !.
cmmmc(A, B, N, M, R):-
    N < M, !,
    N1 is N + A,
    cmmmc(A, B, N1, M, R).
cmmmc(A, B, N, M, R):-
    N > M,
    M1 is M + B,
    cmmmc(A, B, N, M1, R).
cmmmc(A,B,R):- cmmmc(A, B, A, B, R).
         

%3.	Calculați divizorii unui număr natural.
%?-  divisor(15,R1), divisor(2,R2), divisor(1,R3), divisor(0,R4),divisor(6,R5). 
%R1 = [1,3,5,15], R2 = [1,2], R3 = [1], R4 = alot, R5 = [1,2,3,6]. 

divisor(N, N, [N]):- !.
divisor(N, I, [I|R]):-
    0 is N mod I, !,
    I1 is I + 1,
    divisor(N, I1, R).
divisor(N, I, R):-
    I1 is I + 1,
    divisor(N, I1, R).
divisor(N, R):- divisor(N, 1, R).
    

%4.	Convertiți un număr în binar (puterile lui 2 cresc de la dreapta la stânga). 
%?- to_binary(5,R1),to_binary(8,R2),to_binary(11,R3). 
%R1 = [1,0,1], R2 = [1,0,0,0], R3 = [1,0,1,1]. 

to_binary(0, R, R):- !.
to_binary(N, Acc, R):-
    B is N mod 2,
    N1 is N // 2,
    to_binary(N1, [B|Acc], R).
to_binary(N, R):- to_binary(N, [], R).


%5.	Inversați un număr natural.
%?- reverse(15,R1), reverse(121235124,R2). 
%R1 = 51, R2 = 421542121. 

reverse(0, R, R):- !.
reverse(N, Acc, R):- 
    D is N mod 10,
    N1 is N // 10,
    Acc1 is Acc * 10 + D,
	reverse(N1, Acc1, R).
reverse(N, R):- reverse(N, 0, R).


%--------------------------------------------------
% 2	Operații pe liste %
%--------------------------------------------------


%6.	Calculați suma elementelor unei liste. 
%?- sum([1,2,3,4,5], R).
%R = 15. 

sum([], 0).
sum([H|T], R):-
    sum(T, R1),
    R is R1 + H.


%7.	Dublați elementele impare și ridicați la pătrat cele pare.
%?- numbers([2,5,3,1,1,5,4,2,6],R). 
%R = [4,10,6,2,2,10,16,4,36]. 

numbers_opp(N, R):-
    0 is N mod 2, !,
    R is N * N.
numbers_opp(N, R):-
    R is N * 2.

numbers([], []).
numbers([H|T], [H1|R]):-
	numbers_opp(H, H1),
    numbers(T, R).


%8.	Extrageți numerele pare în E și numerele impare în O (indexarea începe de la 1). 
%?- separate_parity([1,2,3,4,5,6], E, O). 
%E = [2,4,6], O=[1,3,5]. 

separate_parity([], [], []).
separate_parity([H|T], [H|E], O):-
    0 is H mod 2, !,
    separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]):-
    separate_parity(T, E, O).


%9.	Înlocuiți toate aparițiile lui X cu Y.
%?- replace_all(1, a, [1,2,3,1,2], R). 
%R = [a,2,3,a,2]. 
	
replace_all(_, _, [], []).
replace_all(X, NX, [X|T], [NX|R]):- !,
    replace_all(X, NX, T, R).
replace_all(X, NX, [H|T], [H|R]):-
    replace_all(X, NX, T, R).


%10. Înlocuiți toate aparițiile lui X într-o listă diferență (al doilea si al treilea argument) cu secvența [Y,X,Y].
%% replace_all(X, S, E, Y, R). unde lista diferență este S-E = [1,2,3,4,2,1,2]
%?- replace_all(2,[1,2,3,4,2,1,2,2,3],[2,3],8,R). 
%R = [1,8,2,8,3,4,8,2,8,1,8,2,8]. 

replace_all(_, S, E, _, []):- S == E, !.
replace_all(X, [X|S], E, Y, [Y, X, Y|R]):- !,
    replace_all(X, S, E, Y, R).
replace_all(X, [H|S], E, Y, [H|R]):-
    replace_all(X, S, E, Y, R).


%11. Sțergeți aparițiile lui X pe poziții pare (indexarea începe de la 1)
%?- delete_pos_even([1,2,3,4,2,3,3,2,5],2,R). 
%R = [1,3,4,2,3,3,5]. 

delete_pos_even([], _, _, []).
delete_pos_even([H|T], H, Acc, R):-
    0 is Acc mod 2, !,
    Acc1 is Acc + 1,
    delete_pos_even(T, H, Acc1, R).
delete_pos_even([H|T], X, Acc, [H|R]):-
    Acc1 is Acc + 1,
    delete_pos_even(T, X, Acc1, R).
delete_pos_even(L, X, R):- delete_pos_even(L, X, 1, R).


%12. Ștergeți elementele de pe poziții divizibile cu K. 
%?- delete_kth([6,5,4,3,2,1], 3, R). 
%R = [6,5,3,2]. 

delete_kth([], _, _, []).
delete_kth([_|T], K, K, R):- !,
    delete_kth(T, K, 1, R).
delete_kth([H|T], K, I, [H|R]):-
    I1 is I + 1,
    delete_kth(T, K, I1, R).
delete_kth(L, K, R):- delete_kth(L, K, 1, R).


%13. Ștergeți elementele de pe poziții divizibile cu K de la finalul listei. 
%?- delete_kth_end([1,2,3,4,5,6,7,8,9,10],3,R) 
%R = [1,3,4,6,7,9,10]. 

delete_kth_end_update(R, _, K, K, R, 1):- !.
delete_kth_end_update(R, H, _, I, [H|R], I1):-
    I1 is I + 1.

delete_kth_end([], _, 1, []).
delete_kth_end([H|T], K, I, R):-
    delete_kth_end(T, K, I1, R1),
    delete_kth_end_update(R1, H, K, I1, R, I).
    
    
delete_kth_end(L, K, R):- delete_kth_end(L, K, _, R).

%14. Ștergeți toate aparițiile elementului minim/maxim dintr-o listă. 
%?- delete_min([4,5,1,2], R). 
%R = [4,5,2]. 

min14([M], M).
min14([H|T], M):-
    min14(T, M),
    H > M, !.
min14([M|_], M).

delete_all14(_, [], []).
delete_all14(X, [X|T], R):- !,
    delete_all14(X, T, R).
delete_all14(X, [H|T], [H|R]):-
    delete_all14(X, T, R).
delete_min(L, R):- min14(L, M), delete_all14(M, L, R). 


%15. Ștergeți elementele duplicate dintr-o listă (păstrează prima sau ultima apariție). 
%?- delete_duplicates([3,4,5,3,2,4], R). 
%R = [3,4,5,2].  sau  R = [5,3,2,4]. 

:- dynamic seen15/1.

delete_duplicates([], []).
delete_duplicates([H|T], [H|R]):-
    not(seen15(H)), !,
    assert(seen15(H)),
    delete_duplicates(T, R).
delete_duplicates([_|T], R):-
    delete_duplicates(T, R).


%16. Inversează o listă incompletă.
%?- reverse16([1, 2, 3, 4, 5|_], R). 
%R = [5, 4, 3, 2, 1|_]. 

reverse16(L, R, R):- var(L), !.
reverse16([H|T], Acc, R):-
    reverse16(T, [H|Acc], R).
reverse16(L, R):- reverse16(L, _, R).


%17. Inversați elementele dintr-o lista după poziția K.
%?- reverse_k17([1,2,3,4,5,6], 2, R). 
%R = [1,2,6,5,4,3]. 

reverse17([], R, R).
reverse17([H|T], Acc, R):-
    reverse17(T, [H|Acc], R).
reverse17(L, R):- reverse17(L, [], R).

reverse_k17(L, 0, R):- !,
    reverse17(L, R).
reverse_k17([H|T], K, [H|R]):-
    K1 is K - 1,
    reverse_k17(T, K1, R).


%18. Codificați o listă cu RLE (Run-length encoding). Două sau mai multe elemente consecutive se
%înlocuiesc cu (element, nr_apariții).
%?- rle_encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e], R).
%R = [[a,4], [b,1] ,[c,2], [a,2], [d,1] , [e,4]].

rle_encode([], _, []).
rle_encode([H,H|T], Cnt, R):- !,
    Cnt1 is Cnt + 1,
    rle_encode([H|T], Cnt1, R).
rle_encode([H|T], Cnt, [[H,Cnt]|R]):-
    rle_encode(T, 1, R).

rle_encode([], []).
rle_encode(L, R):- rle_encode(L, 1, R).

%19. Codificați o listă cu RLE (Run-length encoding). Două sau mai multe elemente consecutive se
%înlocuiesc cu (element, nr_apariții). Dar dacă numărul de apariții este egal cu 1 atunci se
%scrie doar elementul.
%?- rle_encode1([1,1,1,2,3,3,4,4], R).
%R = [(1,3), 2, (3,2), (4,2)].

add19(L, X, 1, [X|L]):- !.
add19(L, X, Cnt, [(X, Cnt)|L]).

rle_encode1([], _, []).
rle_encode1([H,H|T], Cnt, R):- !,
    Cnt1 is Cnt + 1,
    rle_encode1([H|T], Cnt1, R).
rle_encode1([H|T], Cnt, R):-
    rle_encode1(T, 1, R1),
    add19(R1, H, Cnt, R).

rle_encode1([], []).
rle_encode1(L, R):- rle_encode1(L, 1, R).


%20. Decodificați o listă cu RLE (Run-length encoding).
%?- rle_decode([[a,4], [b,1] ,[c,2], [a,2], [d,1] , [e,4]],R). 
%R = [a,a,a,a,b,c,c,a,a,d,e,e,e,e]. 

rle_decode([], []).
rle_decode([[X,1]|T], [X|R]):- !,
    rle_decode(T, R).
rle_decode([[X,Cnt]|T], [X|R]):-
    Cnt1 is Cnt - 1,
    rle_decode([[X,Cnt1]|T], R).

%21. Rotiți lista K poziții în dreapta.
%?- rotate_k([1,2,3,4,5,6|_], 2, R). 
%R = [5,6,1,2,3,4|_]. 

split_k(List, _, _, _):- var(List), !.
split_k(List, 0, _, List):- !.
split_k([H|T], K, [H|L], R):-
    K1 is K - 1,
    split_k(T, K1, L, R).

len21(List, 0):- var(List), !.
len21([_|T], R):-
    len21(T, R1),
    R is R1 + 1.

append21(L1, R, R):- var(L1), !.
append21([H|T], L2, [H|R]):-
    append21(T, L2, R).
    
rotate_k(List, K, Result):- 
    len21(List, N),
    Kaux = K mod N,
    K1 is N - Kaux,
    split_k(List, K1, L, R),
    append21(R, L, Result).
    


%22. Sortați o listă de caractere în funcție de codul ASCII.
%?- sort_chars([e, t, a, v, f], R). 
%R = [a, e, f, t, v]. 

partition_chars(_, [], [], []).
partition_chars(X, [H|T], Sm, [H|Gt]):-
    char_code(X, CX),
    char_code(H, CH),
    CH > CX, !,
    partition_chars(X, T, Sm, Gt).
partition_chars(X, [H|T], [H|Sm], Gt):-
    partition_chars(X, T, Sm, Gt).

sort_chars([], []).
sort_chars([H|T], R):-
    partition_chars(H, T, Sm, Gt),
    sort_chars(Sm, SmR),
    sort_chars(Gt, GtR),
    append(SmR, [H|GtR], R).


%23. Sortați o listă de liste în funcție de lungimea listelor de nivel 2. 
%?- sort_len([[a, b, c], [f], [2, 3, 1, 2], [], [4, 4]], R). 
%R = [[], [f], [4, 4], [a, b, c], [2, 3, 1, 2]]. 

partition_len(_, [], [], []).
partition_len(X, [H|T], Sm, [H|Gt]):-
    length(X, LX),
    length(H, LH),
    LH > LX, !,
    partition_len(X, T, Sm, Gt).
partition_len(X, [H|T], [H|Sm], Gt):-
    partition_len(X, T, Sm, Gt).

sort_len([], []).
sort_len([H|T], R):-
    partition_len(H, T, Sm, Gt),
    sort_len(Sm, SmR),
    sort_len(Gt, GtR),
    append(SmR, [H|GtR], R).


%24. Stergeți elementele duplicate de pe poziții impare dintr-o listă (indexarea începe de la 1). 
%?- remove_dup_on_odd_pos([1,2,3,1,3,3,3,9,10,6,10,8,7,3],R). 
%R = [2,1,3,9,6,8,7,3]. 

:- dynamic count/2.
count23([]).
count23([H|T]):-
    retract(count(H, Cnt)), !,
    Cnt1 is Cnt + 1,
    assert(count(H, Cnt1)),
    count23(T).
count23([H|T]):-
    assert(count(H, 1)),
    count23(T).

remove_dup_on_odd_pos([], _, []).
remove_dup_on_odd_pos([H|T], I, R):-
    1 is I mod 2,
    count(H, Cnt),
    Cnt > 1, !,
    I1 is I + 1,
    remove_dup_on_odd_pos(T, I1, R).
remove_dup_on_odd_pos([H|T], I, [H|R]):-
    I1 is I + 1,
    remove_dup_on_odd_pos(T, I1, R).

remove_dup_on_odd_pos(L, R):-
    count23(L),
    remove_dup_on_odd_pos(L, 1, R).


%--------------------------------------------------
% 3	Deep Lists %
%--------------------------------------------------
%25. Calculați adâncimea maximă a unei liste imbricate. 
%?- depth_list([1, [2, [3]], [4]], R1), depth_list([], R2). 
%R1 = 3, R2 = 1. 

max(A, B, A):-
    A > B, !.
max(_, B, B).

depth_list([], 1).
depth_list([H|T], R):-
    atomic(H), !,
    depth_list(T, R).
depth_list([H|T], R):-
    depth_list(H, RH),
    depth_list(T, RT),
    R1 is RH + 1,
    max(R1, RT, R).


%26. Aplatizați o listă imbricată cu liste complete/incomplete. 
%?- flatten([[1|_], 2, [3, [4, 5|_]|_]|_], R). 
%R = [1,2,3,4,5|_]. 

append_il(L1, L2, L2):- var(L1), !.
append_il([H|T], L2, [H|R]):-
    append_il(T, L2, R).

flatten(L, _):- var(L), !.
flatten([H|T], [H|R]):-
    atomic(H), !,
    flatten(T, R).
flatten([H|T], R):-
    flatten(H, RH),
    flatten(T, RT),
    append_il(RH, RT, R).


%27. Aplatizați doar elementele de la o adâncime dată într-o listă imbricată. 
%?- flatten_only_depth([[1,5,2,4],[1,[4,2],[5,[6,7,8]]],[4,[7]],8,[11]],3,R). 
%R = [4,2,5,7]. 

flatten_only_depth([], _, []):- !.
flatten_only_depth(_, 0, []):- !.
flatten_only_depth([H|T], 1, [H|R]):-
    atomic(H), !,
    flatten_only_depth(T, 1, R).
flatten_only_depth([H|T], D, R):-
    atomic(H), !,
    flatten_only_depth(T, D, R).
flatten_only_depth([H|T], D, R):-
    D1 is D - 1,
    flatten_only_depth(H, D1, RH),
    flatten_only_depth(T, D, RT),
    append(RH, RT, R).


%28. Calculați suma elementelor de la nivelul K intr-o lista imbricată. 
%?- sum_k([1, [2, [3|_]|_], [4|_]|_], 2, R). 
%R = 6. 

sum_k(L, _, 0):- var(L), !.
sum_k(_, 0, 0):- !.
sum_k([H|T], 1, R):-
    atomic(H), !,
    sum_k(T, 1, R1),
    R is R1 + H.
sum_k([H|T], D, R):-
    atomic(H), !,
    sum_k(T, D, R).
sum_k([H|T], D, R):-
    D1 is D - 1,
    sum_k(H, D1, R1),
    sum_k(T, D, R2),
    R is R1 + R2.


%29. Calculați numărul de liste într-o listă imbricată.  
%?- count_lists([[1,5,2,4],[1,[4,2],[5]],[4,[7]],8,[11]],R). 
%R = 8. 

count_lists_aux([], 0).
count_lists_aux([H|T], R):-
    atomic(H), !,
    count_lists_aux(T, R).
count_lists_aux([H|T], R):-
    count_lists_aux(H, R1),
    count_lists_aux(T, R2),
    R is R1 + R2 + 1.
count_lists(L, R):-
    count_lists_aux(L, R1),
    R is R1 + 1.


%30. Înlocuiți toate aparițiile lui X cu Y în lista imbricată. 
% ?- replace_all30(2, 5, [[1, [2, [3, 2]], [4]]], R). 
%R = [1, [5, [3, 5]], [4]]. 

replace_all30(_, _, [], []):- !.
replace_all30(X, Y, [X|T], [Y|R]):-
    !,
    replace_all30(X, Y, T, R).
replace_all30(X, Y, [H|T], [H|R]):-
    atomic(H), !,
    replace_all30(X, Y, T, R).
replace_all30(X, Y, [H|T], [RH|RT]):-
    replace_all30(X, Y, H, RH),
    replace_all30(X, Y, T, RT).


%31. Înlocuiți fiecare secvență cu o adâncime constantă cu lungimea într-o listă adâncă.
%?- len_con_depth([[1,2,3],[2],[2,[2,3,1],5],3,1],R). 
%R = [[3],[1],[1,[3],1],2]. 

add31(L, 0, L):- !.
add31(L, V, [V|L]).

len_con_depth([], 0, []).
len_con_depth([H|T], Cnt, R):-
    atomic(H), !,
    len_con_depth(T, Cnt1, R),
    Cnt is Cnt1 + 1.
len_con_depth([H|T], 0, [RH|RT]):-
    len_con_depth(H, CntH, RH1),
    len_con_depth(T, CntT, RT1),
    add31(RH1, CntH, RH),
    add31(RT1, CntT, RT).
len_con_depth(L, R):-
    len_con_depth(L, Cnt, R1),
    add31(R1, Cnt, R).

%--------------------------------------------------
% 4	Trees %
%--------------------------------------------------
%32. Calculați adâncimea unui arbore binar complet/incomplet. 
%?- tree_ex32(T), depth_tree(T, R). 
%R = 3. 

tree_ex32(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))). 

depth_tree(nil, 0).
depth_tree(t(_, L, R), DR):-
    depth_tree(L, LDR),
    depth_tree(R, RDR),
    max(LDR, RDR, DR1),
    DR is DR1 + 1.


%33. Colectați toate nodurile unui arbore binar complet/incomplet în inordine folosind liste complete. 
%?- tree_ex33(T), inorder(T, R). 
%R = [2,4,5,6,7,9]. 

tree_ex33(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))). 

inorder(nil, []).
inorder(t(K, NL, NR), R):-
    inorder(NL, LR),
    inorder(NR, RR),
    append(LR, [K|RR], R).


%34. Colectați toate frunzele dintr-un arbore binar
%?- tree_ex34(T), collect_k(T, R). 
%R = [2,5,7]. 

tree_ex34(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))). 

collect_k(nil, []).
collect_k(t(K, nil, nil), [K]):- !.
collect_k(t(_, NL, NR), R):-
    collect_k(NL, LR),
    collect_k(NR, RR),
    append(LR, RR, R).


%35. Scrieți un predicat care verifică dacă un arbore este arbore binar de căutare.
%?- tree_ex35(T), is_bst(T). 
%false.

tree_ex35(t(3, t(2, t(1, nil, nil), t(4, nil, nil)), t(5, nil, nil))). 

inorder35(nil, []).
inorder35(t(K, L, R), List):-
    inorder35(L, LList),
    inorder35(R, RList),
    append(LList, [K|RList], List).
check([]).
check([_]).
check([H1,H2|T]):-
    H1 =< H2,
    check([H2|T]).
is_bst(T):-
	inorder35(T, L),
    check(L).

%36. Arbore binar imcomplet. Colectați nodurile impare cu un singur copil într-o listă incompletă.
%?- tree_ex36(X), collect_odd_from_1child(X,R). 
%R = [35, 51|_].  

tree_ex36(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))). 

append_il36(L1, L2, L2):- var(L1), !.
append_il36([H|T], L2, [H|R]):-
    append_il36(T, L2, R).

collect_odd_from_1child(T, _):- var(T), !.
collect_odd_from_1child(t(K, L, R), Res):-
    1 is K mod 2,
    nonvar(L),
    var(R), !,
    collect_odd_from_1child(L, LRes),
    append_il36(LRes, [K|_], Res).
collect_odd_from_1child(t(K, L, R), [K|RRes]):-
    1 is K mod 2,
    nonvar(R),
    var(L), !,
    collect_odd_from_1child(R, RRes).
collect_odd_from_1child(t(_, L, R), Res):-
    collect_odd_from_1child(L, LRes),
   	collect_odd_from_1child(R, RRes),
    append_il36(LRes, RRes, Res).


%37. Arbore ternar incomplet. Colectați cheile între X și Y (interval închis) într-o listă diferență.
%?- tree_ex37(T), collect_between(T,2,7,R,[1,18]). 
%R = [2,3,4,5,6,7,1,18]. 

tree_ex37(t(2,t(8,_,_,_),t(3,_,_,t(4,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))). 

%collect_between(T, IS, IE, S, E).
collect_between(T, _, _, E, E):- var(T), !.
collect_between(t(K, L, M, R), IS, IE, S, E):-
    K >= IS,
    K =< IE, !,
    collect_between(L, IS, IE, S, [K|INT1]),
    collect_between(M, IS, IE, INT1, INT2),
    collect_between(R, IS, IE, INT2, E).
collect_between(t(_, L, M, R), IS, IE, S, E):-
    collect_between(L, IS, IE, S, INT1),
    collect_between(M, IS, IE, INT1, INT2),
    collect_between(R, IS, IE, INT2, E).


%38. Arbore binar. Colectați cheile pare ale frunzelor într-o listă diferență. 
%?-  tree_ex38(T), collect_even_from_leaf(T,R,[1]). 
%R = [4,2,16,1]. 

tree_ex38(t(5,t(10,t(7,nil,nil),t(10,t(4,nil,nil),t(3,nil,t(2,nil,nil)))),t(16,nil,nil))).

collect_even_from_leaf(nil, E, E).
collect_even_from_leaf(t(K, nil, nil), [K|E], E):-
    0 is K mod 2, !.
collect_even_from_leaf(t(_, L, R), S, E):-
    collect_even_from_leaf(L, S, INT),
    collect_even_from_leaf(R, INT, E).


%39. Înlocuiți elementul minim dintr-un arbore ternar incomplet cu rădăcina.
%?- tree_ex39(T), replace_min(T,R). 
%R = t(2,t(8,_,_,_),t(3,_,_,t(2,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(2,_,_,t(9,_,_,_))))  

tree_ex39(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))). 

min39(X, Y, _):- var(X), var(Y), !.
min39(X, Y, X):- nonvar(X), var(Y), !.
min39(X, Y, Y):- var(X), nonvar(Y), !.
min39(X, Y, Y):- X > Y, !.
min39(X, _, X).

find_min(T, _):- var(T), !.
find_min(t(K, L, M, R), MIN):-
    find_min(L, LM),
    find_min(M, MM),
    find_min(R, RM),
    min39(LM, MM, AM1),
    min39(AM1, RM, AM2),
    min39(AM2, K, MIN).

replace39(T, _, _, _):- var(T), !.
replace39(t(X, L, M, R), X, Y, t(Y, NL, NM, NR)):- !,
    replace39(L, X, Y, NL),
    replace39(M, X, Y, NM),
    replace39(R, X, Y, NR).
replace39(t(K, L, M, R), X, Y, t(K, NL, NM, NR)):-
    replace39(L, X, Y, NL),
    replace39(M, X, Y, NM),
    replace39(R, X, Y, NR).

replace_min(t(ROOT, L, M, R), Result):-
    find_min(t(ROOT, L, M, R), MIN),
    replace39(t(ROOT, L, M, R), MIN, ROOT, Result).


%40. Colectați toate nodurile de la adâncimea K dintr-un arbore binar.  
%?- tree_ex40(T), collect_k(T, 2, R). 
%R = [4, 9]. 

tree_ex40(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).

collect_k(nil, _, []).
collect_k(t(K, _, _), 1, [K]):- !.
collect_k(t(_, L, R), D, Result):-
    D1 is D - 1,
    collect_k(L, D1, LR),
    collect_k(R, D1, RR),
    append(LR, RR, Result).


%41. Colectați toate nodurile de la adâncimi impare dintr-un arbore binar incomplet (rădăcina are adâncime 0).
%?- tree_ex41(X), collect_all_odd_depth(X,R). 
%R = [14,50,29,58]. 

tree_ex41(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))). 

collect41(nil, _, []):- !.
collect41(t(_, L, R), 0, Result):- !,
    collect41(L, 1, LR),
    collect41(R, 1, RR),
    append(LR, RR, Result).
collect41(t(K, L, R), 1, Result):-
    collect41(L, 0, LR),
    collect41(R, 0, RR),
    append([K|LR], RR, Result).

collect_all_odd_depth(T, R):-
    collect41(T, 0, R).


%42. Colectați subarborii cu rădăcini conținând valoarea mediană dintr-un arbore ternar incomplet.
% Observație. Mediana este "mijlocul" listei sortate de chei
%?- tree_ex42(T), median(T,R). 
%R = [
%	t(5,t(7,_,_,_),t(5,_,_,_),t(1,_,_,t(9,_,_,_)))), 
%	t(5,_,_,_)
%].

tree_ex42(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(5,_,_,_),t(1,_,_,t(9,_,_,_))))). 

append3(L1, L2, L3, R):-
    append(L2, L3, R1),
    append(L1, R1, R).

collect42(T, []):- var(T), !.
collect42(t(K, L, M, R), Result):-
    collect42(L, LR),
    collect42(M, MR),
    collect42(R, RR),
    append3([K|LR], MR, RR, Result).

partition42(_, [], [], []).
partition42(X, [H|T], [H|Sm], Gt):-
	H < X, !,
    partition42(X, T, Sm, Gt).
partition42(X, [H|T], Sm, [H|Gt]):-
    partition42(X, T, Sm, Gt).
   
sort42([], []).
sort42([H|T], R):-
    partition42(H, T, Sm, Gt),
    sort42(Sm, SmR),
    sort42(Gt, GtR),
    append(SmR, [H|GtR], R).

get([], _, _).
get([H|_], 0, H):- !.
get([_|T], K, R):-
    K1 is K - 1,
    get(T, K1, R).

collect_subtree42(T, _, []):- var(T), !.
collect_subtree42(t(K, L, M, R), K, Result):- !,
    collect_subtree42(L, K, LR),
    collect_subtree42(M, K, MR),
    collect_subtree42(R, K, RR),
    append3([t(K, L, M, R)|LR], MR, RR, Result).
collect_subtree42(t(_, L, M, R), X, Result):-
    collect_subtree42(L, X, LR),
    collect_subtree42(M, X, MR),
    collect_subtree42(R, X, RR),
    append3(LR, MR, RR, Result).

median(T, R):-
    collect42(T, Values),
    sort42(Values, OrderedValues),
    length(OrderedValues, N),
    Middle is N // 2,
    get(OrderedValues, Middle, Val),
	collect_subtree42(T, Val, R).


%43. Înlocuiți fiecare nod cu înalțimea într-un arbore binar incomplet (frunzele au înalțimea 0) 
%?- tree_ex43(T), height_each(T,R). 
%R = tree_ex43(t(3,t(1,t(0,_,_),t(0,_,_)),t(2,t(1,t(0,_,_),_),t(1,_,t(0,_,_))))). 

tree_ex43(t(2,t(4,t(5,_,_),t(7,_,_)),t(3,t(0,t(4,_,_),_),t(8,_,t(5,_,_))))). 

max43(X, Y, Y):- Y > X, !.
max43(X, _, X).

height_each(t(_, L, R), t(0, _, _)):- var(L), var(R), !.
height_each(t(_, L, R), t(H, t(LH, LL, LR), _)):-
    var(R), nonvar(L), !,
    height_each(L, t(LH, LL, LR)),
    H is LH + 1.
height_each(t(_, L, R), t(H, _, t(RH, RL, RR))):-
    var(L), nonvar(R), !,
    height_each(R, t(RH, RL, RR)),
    H is RH + 1.
height_each(t(_, L, R), t(H, t(LH, LL, LR), t(RH, RL, RR))):-
    height_each(L, t(LH, LL, LR)),
    height_each(R, t(RH, RL, RR)),
    max43(LH, RH, H1),
    H is H1 + 1.


%44. Scrieți un predicat care înlocuiește întregul subarbore al unui nod (cu o cheie dată ca
% argument) cu un singur nod care are cheia suma cheilor subarborelui acelui nod (dacă nu
% există un nod cu aceea cheie, rămâne neschimbat).
%?- tree_ex44(T), sum_subtree(T,6,R). 
%R = t(14,t(32,nil,nil),t(17,t(16,nil,nil),t(20,nil,nil)))).

tree_ex44(t(14,t(6,t(4,nil,nil),t(12,t(10,nil,nil),nil)),t(17,t(16,nil,nil),t(20,nil,nil)))).

sum44(nil, 0).
sum44(t(K, L, R), S):-
    sum44(L, LS),
    sum44(R, RS),
    S is LS + RS + K.

sum_subtree(nil, _, nil).
sum_subtree(t(K, L, R), K, t(S, nil, nil)):- !,
    sum44(t(K, L, R), S).
sum_subtree(t(K, L, R), X, t(K, NL, NR)):-
    sum_subtree(L, X, NL),
    sum_subtree(R, X, NR).


%--------------------------------------------------
% 5	Graphs %
%--------------------------------------------------
%45. Colectați toate nodurile unui graf. 
%Ex: node(1). node(2). node(3). 
%?- collect(R). 
%R = [1,2,3].


node(1). 
node(2). 
node(3). 

:- dynamic seen/1.

collect([X|R]):-
    node(X),
    not(seen(X)),
	assert(seen(X)), !,
    collect(R).
    
collect([]).

%46. Calculați gradul interior/exterior al fiecărui nod dintr-un graf folosind predicatul dinamic info(Node, OutDegree, InDegree).  
%Ex: edge(1,2). edge(2,1). edge(1,4). edge(1,3). edge(3,2). 
%=> info(1,3,1). info(2,1,2). info(3,1,1). info(4,0,1).

edge(1,2).
edge(2,1).
edge(1,4).
edge(1,3).
edge(3,2). 

:- dynamic node46/1.

update_in_degree(X):-
    retract(info(X, OutDegree, InDegree)), !,
    InDegree1 is InDegree + 1,
    assert(info(X, OutDegree, InDegree1)).

update_in_degree(X):-
    assert(info(X, 0, 1)).

update_out_degree(X):-
    retract(info(X, OutDegree, InDegree)), !,
	OutDegree1 is OutDegree + 1,
    assert(info(X, OutDegree1, InDegree)).

update_out_degree(X):-
    assert(info(X, 1, 0)).

compute_degrees:-
    edge(X, Y),
    update_out_degree(X),
    update_in_degree(Y),
    fail.
compute_degrees.

collect_info([i(X, Out, In)|R]):-
    retract(info(X, Out, In)), !,
    collect_info(R).
    
collect_info([]).

solve(R):-
    compute_degrees,
    collect_info(R).
