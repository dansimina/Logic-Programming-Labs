edge(1,2).
edge(1,5).
edge(2,3).
edge(2,5).
edge(3,4).
edge(4,5).
edge(4,6).

:- dynamic visited/1.
:- dynamic queue/1.

is_edge(X, Y):- (edge(X, Y); edge(Y, X)).

expand(X):-
    is_edge(X, Y),
    not(visited(Y)),
    assertz(visited(Y)),
    assertz(queue(Y)),
	fail.
expand(_).  

bfs_traversal:-
    retract(queue(X)), !,
    expand(X),
    bfs_traversal.

collect([X|R]):-
    retract(visited(X)), !,
    collect(R).
collect([]).

collect_with_findall(R):-
    findall(X, visited(X), R).

bfs(X, _):- 
    assertz(visited(X)),
    assertz(queue(X)),
    bfs_traversal.
bfs(_, R):- collect_with_findall(R).

dfs_traversal(X):-
    assert(visited(X)),
    is_edge(X, Y),
    not(visited(Y)),
    dfs_traversal(Y).

dfs(X, _):- dfs_traversal(X).
dfs(_, R):- collect_with_findall(R).

%S = 11, V = [10, 5, 2, 1]
aux([H|T], H, T).
aux([_|T], RH, RT):-
    aux(T, RH, RT).

find_numbers_with_sum_s(S, _, S, []):- !.
find_numbers_with_sum_s(S, V, Acc, [H|R]):-
	Acc < S,
    aux(V, H, T),
    Acc1 is Acc + H,
    find_numbers_with_sum_s(S, [H|T], Acc1, R).
find_numbers_with_sum_s(S, V, R):-
    find_numbers_with_sum_s(S, V, 0, R).

%group([a, b, c, d, e, f, g, h, i], [2, 2, 5], R)
%[[a, b], [c, d], [e, f, g, h, i]]
%[[a, c], [b, d], [e, f, g, h, i]]
%...

select_aux([H|T], H, T).
select_aux([_|T], RH, RT):-
    select_aux(T, RH, RT).

select_k(_, 0, []).
select_k(L, K, [H|R]):-
    K1 is K - 1,
    select_aux(L, H, T),
	select_k(T, K1, R).

remove_selected([], _, []).
remove_selected([H|T], S, [H|R]):-
    \+member(H, S), !,
   	remove_selected(T, S, R).
remove_selected([_|T], S, R):-
    remove_selected(T, S, R).

group(_, [], []).
group(L, [K|T], [S|R]):-
    select_k(L, K, S),
    remove_selected(L, S, L1),
    group(L1, T, R).

delete1(_, [], []).
delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]):-
    delete1(X, T, R).

%perm([1,2,3], R).
perm([], []):- !.
perm(L, [X|R]):- 
	delete1(X, L, L1),
    nonvar(X),
    perm(L1, R).

%
tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).

pretty_print(T):- pretty_print(T, 0).

pretty_print(nil, _).
pretty_print(t(K,L,R), D):- 
	D1 is D+1,
	pretty_print(L, D1),
	print_key(K, D),
	pretty_print(R, D1).

print_key(K, D):-D>0, !, D1 is D-1, tab(8), print_key(K, D1).
print_key(K, _):-write(K), nl.

%
rotate(t(K, L, t(RK, RL, RR)), T):-
    !, rotate(t(RK, t(K, L, RL), RR), T).
rotate(T, T).

tree_to_chain(nil, nil):- !.            
tree_to_chain(T, t(NK, NNL, NR)):-
    rotate(T, t(NK, NL, NR)),
    tree_to_chain(NL, NNL).

%

:- dynamic q/1, set/2.

edge_bip(1,2).
edge_bip(2,3).
edge_bip(3,4).
edge_bip(1,4).

is_edge_bip(X, Y):- edge_bip(X, Y); edge_bip(Y, X).

neg(white, black).
neg(black, white).

check(X, Y):-
    set(X, Xc),
    set(Y, Yc),
    Xc \= Yc, !.
check(X, Y):-
	set(X, Xc),
    not(set(Y, _)),
    neg(Xc, Yc),
    assertz(q(Y)),
    assertz(set(Y, Yc)).

expand_bipartite(X):-
    is_edge_bip(X, Y),
    \+check(X, Y), !,
    fail.
expand_bipartite(_).

bfs_bipartite:-
    retract(q(X)), !,
    expand_bipartite(X),
    bfs_bipartite.
        
bipartite(X, _, _):-
 	assertz(q(X)),
    assertz(set(X, white)),
   	bfs_bipartite.
bipartite(_, S1, S2):-
    not(q(_)),
    findall(X, set(X, white), S1),
    findall(X, set(X, black), S2).

%

%?- distribute_by_weight([(a,2),(b,3),(c,1),(d,4),(e,2),(f,3)], [5,4,6], R).
%R = [[(a,2),(b,3)], [(c,1)], [(d,4),(e,2)]]

select_under_weight([(X, W)|T], K, [(X, W)|RL], RR):-
    K1 is K - W,
    K1 >= 0, !,
   	select_under_weight(T, K1, RL, RR).
select_under_weight([], K, [], []):-
    K >= 0, !.
select_under_weight(L, _, [], L).

distribute_by_weight(_, [], []).
distribute_by_weight(L, [K|T], [H|R]):-
    select_under_weight(L, K, H, L1),
    distribute_by_weight(L1, T, R).

%?- tree_hb1(T), collect_balanced_subtrees(T, L).
%T = t(1, t(2, t(4, nil, nil), t(5, nil, nil)), t(3, nil, t(6, nil, nil))),
%L = [t(1, t(2, t(4, nil, nil), t(5, nil, nil)), t(3, nil, t(6, nil, nil))),
%     t(2, t(4, nil, nil), t(5, nil, nil)),
%     t(4, nil, nil),
%     t(5, nil, nil),
%     t(3, nil, t(6, nil, nil)),
%     t(6, nil, nil)].

tree_hb1(t(1,t(2,t(4,nil,nil),t(5,nil,nil)),t(3,nil,t(6,nil,nil)))).

max_hb1(X, Y, X):- X > Y, !.
max_hb1(_, Y, Y).

aux(T, H1, H2, R1, R2, R):-
    D is H1 - H2,
    D >= -1,
    D =< 1, !,
    append([T|R1], R2, R).
aux(_, _, _, R1, R2, R):-
    append(R1, R2, R).

collect_balanced_subtrees(nil, 0, []).
collect_balanced_subtrees(t(K, L, R), H, Result):-
    collect_balanced_subtrees(L, LH, LR),
    collect_balanced_subtrees(R, RH, RR),
    aux(t(K, L, R), LH, RH, LR, RR, Result),
    max_hb1(LH, RH, MH),
    H is MH + 1.

collect_balanced_subtrees(T, L):-
    collect_balanced_subtrees(T, _, L).


%?- extract_above_threshold([5, [3, 8], 1, [12, [2]]], 2, R).
%R = [5, [3, 8], [12, [2]]].

update(H, K, I, R, [H|R]):-
    T is K * I,
    H > T, !.
update(_, _, _, R, R).

extract_above_threshold([], _, _, []).
extract_above_threshold([H|T], K, I, R):-
    atomic(H), !,
    I1 is I + 1,
    extract_above_threshold(T, K, I1, R1),
    update(H, K, I, R1, R).
extract_above_threshold([H|T], K, I, [HR|HT]):-
    I1 is I + 1,
    extract_above_threshold(H, K, 0, HR),
    extract_above_threshold(T, K, I1, HT).
    
extract_above_threshold(L, K, R):-
    extract_above_threshold(L, K, 0, R).

%?- tree_anc(T), anc_sum(T, R).
%R = t(2, t(10, _, _, _), t(5, _, _, t(6, _, _, _)), t(7, t(14, _, _, _), t(8, _, _, t(17, _, _, _)))),
%T = t(2, t(8, _, _, _), t(3, _, _, t(1, _, _, _)), t(5, t(7, _, _, _), t(5, _, _, _), t(1, _, _, t(9, _, _, _))))

tree_anc(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(5,_,_,_),t(1,_,_,t(9,_,_,_))))).

anc_sum(T, _, _):- var(T), !.
anc_sum(t(K, L, M, R), S, t(NS, NL, NM, NR)):-
   	NS is S + K,
    anc_sum(L, NS, NL),
    anc_sum(M, NS, NM),
    anc_sum(R, NS, NR).
anc_sum(T, R):-
    anc_sum(T, 0, R).

% desc([a, b, c, d, e], R).
% R = [[a, b, c], [a, c, d], [a, d, e]]

take(X, [X|T], [], T).
take(X, [H|T], [H|RL], RR):-
	take(X, T, RL, RR).

desc(_, _, [], E, E).
desc(A, B, L, [[A, B, X]|S], E):-
    take(X, L, L1, L2),
    desc(A, X, L2, S, Int),
    desc(X, B, L1, Int, E).

desc(L, R):-
    take(A, L, LA, RA),
    take(B, RA, LB, RB),
    append(LB, RB, L1),
    append(LA, L1, L2),
    desc(A, B, L2, R, []).
