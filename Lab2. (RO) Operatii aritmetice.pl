%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 			LABORATORUL 2 EXEMPLE		%%%%%%
%%%%%% 			Operații aritmetice			%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% The IS predicate %
%--------------------------------------------------
% Urmărește intrebările:

% ?- X = 1+2.
% X = 1+2.

% ?- X is 1+2.
% X = 3.

% Expresia matematică se scrie în dreapta operatului „is” și nu trebuie să conțină variabile neinițializate.
% Exemple:
% ?- X is sqrt(4).
% X = 2.0.

% ?- Y = 10, X is Y mod 2.
% X = 0.

% ?- X is (1+2*3)/7-1.
% X = 0.

% ?- X is Y+1.
% Arguments are not sufficiently instantiated *ERROR*

% ?- X is 1+2, X is 3+4.
% false.


%--------------------------------------------------
% Predicatul CMMDC %
%--------------------------------------------------
% Varianta 1
cmmdc1(X,X,X). % parametrul 3 este rezultatul la cmmdc
cmmdc1(X,Y,Z) :- X>Y, Diff is X-Y, cmmdc1(Diff,Y,Z).
cmmdc1(X,Y,Z) :- X<Y, Diff is Y-X, cmmdc1(X,Diff,Z).
% Varianta 2
cmmdc2(X,0,X). % parametrul 3 este rezultatul la cmmdc
cmmdc2(X,Y,Z) :- Y\=0, Rest is X mod Y, cmmdc2(Y,Rest,Z).

% Urmărește intrebările:
% ?- cmmdc1(30,24,X).
% ?- cmmdc1(15,2,X).
% ?- cmmdc1(4,1,X).





%--------------------------------------------------
% Predicatul FACTORIAL %
%--------------------------------------------------
% Variant 1 - using backwards recursion
fact_bwd(0,1).
fact_bwd(N,F) :- N > 0, N1 is N-1, fact_bwd(N1,F1), F is N*F1.


% Variant 2 - using forwards recursion
fact_fwd(0,Acc,F) :- F = Acc.
fact_fwd(N,Acc,F) :- N > 0, N1 is N-1, Acc1 is Acc*N, fact_fwd(N1,Acc1,F).

% the wrapper
fact_fwd(N,F) :- fact_fwd(N,1,F). % the accumulator is initialized with 1



 
 
 
 
 
 
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% 				EXERCIȚII				%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------
% 1. Scrieți predicatul cmmmc/3 care calculează cel mai mic multiplu comun (CMMMC).
% Sugestie: CMMMC între două numere naturale este raportul dintre produsul lor și CMMDC.
% ?- cmmmc(12, 24, Z).
% Z=24;
% false.

% cmmc(X, Y, Z). - calculeză cel mai mic multiplu comun dintre X și Y și returneză rezultatul în Z

cmmmc(X, Y, Z):- cmmdc2(X, Y, CMMDC), Z is (X * Y) / CMMDC.




%--------------------------------------------------
% 2. Scrieți predicatul triangle/3 care verifică dacă cei trei parametrii pot
% reprezenta lungimile laturilor unui triunghi. Suma oricăror două laturi
% trebuie să fie mai mare decât a treia latură.
% ?- triangle(3, 4, 5).
% true.
% ?- triangle(3, 4, 8).
% false.


% triangle(A, B, C). – va verifica dacă A,B,C ar putea fi laturile unui triunghi și va returna true sau false

triangle(A, B, C):- AB is A + B, BC is B + C, AC is A + C, A < BC, B < AC, C < AB.




%--------------------------------------------------
% 3. Scrieți predicatul solve/4 care să rezolve ecuația de gradul doi (a*x2+b*x+c = 0). Predicatul 
% trebuie să aibă 3 parametri de intrare (A,B,C) și un parametru de ieșire (X). 
% La repetarea întrebării trebuie să se obțină a doua soluție (distinctă) dacă există.
% ?- solve(1, 3, 2, X).
% X=-1;
% X=-2;
% false.
% ?- solve(1, 2, 1, X).
% X=-1;
% false.
% ?- solve(1, 2, 3, X).
% false

% solve(A, B, C, X). – va rezolva ecuația pătratică A*x2+B*x+C = 0
% și va returna rezultatul(ele) în X sau false altfel

sol(A, B, 0, X):- X is -B / (2 * A).
sol(A, B, D, X):- D > 0, (X is (-B + sqrt(D)) / (2 * A); X is (-B - sqrt(D)) / (2 * A)).
solve(A, B, C, X):- D is B * B - 4 * A * C, D >= 0, sol(A, B, D, X).




%--------------------------------------------------
% 4. Scrieți predicatul care calculează ridicarea unui număr la o putere aleasă folosind:
% • recursivitate înainte: power_fwd/3
% • recursivitatea înapoi: power_bwd/3

% ?- power_fwd(2, 3, Z).
% Z=8;
% false.
% ?- power_bwd(2, 3, Z).
% Z=8;
% false.


% power(X, Y, Z). – will calculate X to the power of Y and return it in Z


power_fwd(_, 0, Z, Acc):- Z = Acc.
power_fwd(X, Y, Z, Acc):- Y > 0, Y1 is Y - 1, AccNext is X * Acc, power_fwd(X, Y1, Z, AccNext).
power_fwd(X, Y, Z):- power_fwd(X, Y, Z, 1).

power_bwd(_, 0, 1).
power_bwd(X, Y, Z):- Y > 0, Y1 is Y - 1, power_bwd(X, Y1, Z1), Z is X * Z1.




%--------------------------------------------------
% 5. Scrieți predicatul fib/2 care calculează al n-lea număr din șirul lui Fibonacci.
% Formula de recurență este:
% 	fib(0) = 0
% 	fib(1) = 1
% 	fib(n) = fib(n-1) + fib(n-2), pentru n>0

% ?- fib(5, X).
% X=5;
% false.
% ?- fib(8, X).
% X=21;
% false.


% fib(N, X). – will calculate N-th number of the Fibonacci sequence and return it in X


fib(0, 0).
fib(1, 1). 
fib(N, X):- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, X1), fib(N2, X2), X is X1 + X2.   




%--------------------------------------------------
% 6. Scrieți predicatul anterior folosind doar un singur apel recursiv. 
% ?- fib1(5, X).
% X=5;
% false.
% ?- fib1(8, X).
% X=21;
% false.

fib1(0, X, Acc1, _):- X is Acc1.
fib1(1, X, _, Acc2):- X is Acc2.
fib1(N, X, Acc1, Acc2):- N > 1, Acc3 is Acc1 + Acc2, N1 is N - 1, fib1(N1, X, Acc2, Acc3).
fib1(N, X):- fib1(N, X, 0, 1).




%--------------------------------------------------
% 7. For: Scrieți predicatul for/3 după specificațiile date în secțiunea 2.4 care
% calculează suma tuturor numerelor mai mici decât un număr dat. 
% ?- for(0, S, 9).
% S=45;
% false.


for(Inter,Inter,0).
for(Inter,Out,In):-
	In>0,
	NewIn is In-1,
	% % *IMPLEMENTAȚI AICI* <do_something_to_Inter_to_get_Intermediate>
    Intermediate is Inter + In,
	for(Intermediate,Out,NewIn).




%--------------------------------------------------
% 8. For: Scrieți predicatul for_bwd/2 folosind recursivitate înapoi care
% calculează suma tuturor numerelor mai mici decât un număr dat.
% ?- for_bwd(9, S).
% S=45;
% false.

% for_bwd(In,Out). – va calcula suma toturor valorilor între 0 și In
% și va returna rezultatul în Out

for_bwd(0, 0).
for_bwd(In, Out):- 
    In > 0,
    NewIn is In - 1,
    for_bwd(NewIn, Out1),
    Out is In + Out1.




%--------------------------------------------------
% 9. While: Scrieți predicatul while/3 care simulează o buclă while și
% returnează suma tutoror numerelor între două numere date.
% Sugestie. Structura unei astfel de bucle este:
% 	while <some condition>
% 		<do something>
% 	end while

% ?- while(0, 5, S).
% S=10;
% false.
% ?- while(2, 2, S).
% S=0;
% false


% while(Low,High,Sum). – va calcula suma toturor valorilor între Low și High
% și va returna rezultatul în Sum

while(Low, High, Sum):- 
    Low < High,
    NewLow is Low + 1,
    while(NewLow, High, Sum1),
    Sum is Sum1 + Low.
while(_, _, 0).




%--------------------------------------------------
% 10. Repeat….until: Scrieți predicatul dowhile/3 care simulează o buclă
% repeat…until și returnează suma tutoror numerelor între două numere date.
% Sugestie. Structura unei astfel de bucle este:
% 	repeat
% 		<do something>
% 	until <some condition>

% ?- dowhile(0, 5, S).
% S=10;
% false.
% ?- dowhile(2, 2, S).
% S=2;
% false.


% dowhile(Low,High,Sum). – va calcula suma toturor valorilor între Low și High
% și va returna rezultatul în Sum

dowhile(Low, High, Sum):- 
    Low < High,
    NewLow is Low + 1,
    while(NewLow, High, Sum1),
    Sum is Sum1 + Low.
dowhile(Low, _, Low).
    
    