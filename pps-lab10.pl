%Peano numbers clauses
greater(s(A), zero). 
greater(s(A), s(B)) :- greater(A, B).

greater_equal(A, zero).
greater_equal(s(A), s(B)) :- greater_equal(A, B).

sum(X, zero, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

%-----------------------------------------------------------------------------------------Ex 1-----------------------------------------------------------------------------------------
%Ex 1.1
%search(E, List)
%examples:
%query:
%search(a, cons(a, cons(b, cons(c, nil)))). -> yes
%search(a, cons(c, cons(d, cons(e, nil)))). -> no
%iteration:
%search(X, cons(a, cons(b, cons(c, nil)))). -> yes, three solution
%generation:
%search(a, X). -> yes, infinite solution
%search(a, cons(X, cons(b, cons(Y, cons(Z, nil))))). -> yes, three solution
%search(X, Y). -> yes, infinite solution
search(E, cons(E, _)).
search(E, cons(_ , T)) :- search(E, T).


%Ex 1.2
%search2(E, List)
%examples:
%search2(a, cons(c, cons(a, cons(a, cons(d, cons(a, cons(a, nil))))))). -> yes, two solution
%search2(a, cons(c, cons(a, cons(a, cons(a, nil))))). -> yes, two solution
%search2(a, cons(c, cons(a, cons(a, cons(b, nil))))). -> yes
%search2(a, L). -> yes, infinite solution
%search2(a, cons(_, cons(a, cons(_, cons(a, cons(_, nil)))))). -> yes, four solution
search2(E, cons(E, cons(E, _))).
search2(E, cons(_, T)) :- search2(E, T).

%Ex 1.3
%search_two(E, List)
%examples:
%search_two(a, cons(c, cons(a, cons(a, cons(b, nil))))). -> no
%search_two(a, cons(c, cons(a, cons(d, cons(a, cons(b, nil)))))). -> yes
search_two(E, cons(E, cons(_, cons(E, _)))).
search_two(E, cons(_, T)) :- search_two(E, T).

%Ex 1.4
%search_anytwo(E, List)
%examples:
%search_anytwo(a, cons(c, cons(a, cons(a, cons(b, nil))))). -> yes
%search_anytwo(a, cons(c, cons(a, cons(d, cons(a, cons(b, nil)))))). -> yes
search_anytwo(E, cons(E, T)) :- search(E, T).
search_anytwo(E, cons(_, T)) :- search_anytwo(E, T).

%-----------------------------------------------------------------------------------------Ex 2-----------------------------------------------------------------------------------------
%Ex 2.1
%size(List, S) 
%examples:
%size(cons(a, cons(b, cons(c, nil))), X). -> yes, X/s(s(s(zero)))
%size(X, s(s(s(zero)))). -> yes, X/cons(_e1, cons(_e2, cons(_e3, nil)))
size(nil, zero).
size(cons(_, T), s(N)) :- size(T, N).

%Ex 2.2
%sum_list(List, S)
%examples:
%sum_list(cons(zero, cons(s(s(zero)), cons(s(zero), nil))), X). -> yes, X/s(s(s(zero)))
sum_list(nil, zero).
sum_list(cons(N, T), NS) :- sum_list(T, S), sum(N, S, NS).

%Ex 2.3
%count(List, E, N)
%examples:
%count(cons(a, cons(b, cons(c, cons(a, cons(b, nil))))), a, N). -> yes, N/s(s(zero))
%count(cons(a, cons(b, cons(c, cons(a, cons(b, nil))))), a, zero, N). -> yes, N/s(s(zero))
%count(cons(b, cons(c, cons(a, cons(b, nil)))), a, s(zero), N). -> yes, N/s(s(zero))
%count(cons(c, cons(a, cons(b, nil))), a, s(zero), N). -> yes, N/s(s(zero))
count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

%Ex 2.4
%max(List, Max)
%examples:
%max(cons(s(zero), cons(s(s(s(zero))), cons(s(s(zero)), nil))), X). -> yes, X/s(s(s(zero)))
max(nil, M, M).
max(cons(H,T), TM, M) :- greater(H, TM), max(T, H, M).
max(cons(H,T), TM, M) :- greater_equal(TM, H), max(T, TM, M).
max(cons(H,T), M) :- max(T, H, M).

%Ex 2.5
%min-max(List, Min, Max)
%examples:
%min_max(cons(s(s(zero)), cons(s(s(s(zero))), cons(s(zero), nil))), Min, Max). -> yes, Min/s(zero) Max/s(s(s(zero)))
min_max(cons(H,T), MI, MA) :- min_max(T, H, MI, H, MA).
min_max(nil, MI, MI, MA, MA).
min_max(cons(H,T), TMI, MI, TMA, MA) :- greater(TMI, H), min_max(T, H, MI, TMA, MA).
min_max(cons(H,T), TMI, MI, TMA, MA) :- greater(H, TMA), min_max(T, TMI, MI, H, MA).

%-----------------------------------------------------------------------------------------Ex 3-----------------------------------------------------------------------------------------
%Ex 3.1
%same_list(List, List)
%examples:
%same_list(cons(a, cons(b, nil)), cons(a, cons(b, nil))). -> yes
same_list(nil, nil).
same_list(cons(H1, T1), cons(H1, T1)) :- same_list(T1, T1).
%same(Obj, Obj)
%examples:
%same(cons(a, cons(b, nil)), cons(a, cons(b, nil))). -> yes
%same(ab, ab). -> yes
same(L,L).

%Ex 3.2
%all_bigger(List1, List2)
%examples:
%all_bigger(cons(s(s(zero)), cons(s(zero), nil)), cons(s(zero), cons(zero, nil))). -> yes
all_bigger(nil, nil).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).

%Ex 3.3
%sublist(List1, List2)
%examples:
%sublist(cons(a, cons(b, nil)), cons(c, cons(b, cons(a, nil)))). -> yes
sublist(nil, _).
sublist(cons(H, T), L) :- search(H, L), sublist(T, L). 

%-----------------------------------------------------------------------------------------Ex 4-----------------------------------------------------------------------------------------
%Ex 4.1
%seq(N, E, List)
%examples:
%seq(s(s(zero)), e, cons(e, cons(e, nil))). -> yes
seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

%Ex 4.2
%seqR(N, List).
%examples:
%seqR(s(s(s(zero))), cons(s(s(zero)), cons(s(zero), cons(zero, nil)))). -> yes
seqR(zero, nil).
seqR(s(N), cons(N,T)) :- seqR(N, T).

%Ex 4.3
%seqR2(N, List)
%examples:
%seqR2(s(s(s(zero))), cons(zero, cons(s(zero), cons(s(s(zero)), nil)))). -> yes

%plain implementation
%seqR2(s(N), RL) :- seqR2(s(N), nil, RL).
%seqR2(zero, RL, RL).
%seqR2(s(N), A, RL) :- seqR2(N, cons(N, A), RL).

%implementation with put_last
%put_last(nil, E, cons(E, nil)).
%put_last(cons(H, T1), E, cons(H, T2)) :- put_last(T1, E, T2).
%seqR2(s(N), RL) :- seqR2(s(N), nil, RL).
%seqR2(zero, RL, RL).
%seqR2(s(N), A, RL) :- seqR2(N, A, PRL), put_last(PRL, N, RL). 

%alternative implementation
seqR2(zero, nil).
seqR2(s(N), RL) :- seqR(s(N), L), reversed(L, RL).

%-----------------------------------------------------------------------------------------Ex 5-----------------------------------------------------------------------------------------
%Ex 5.1
%last(?List, ?E)
%find last element in the list
%examples:
%last(cons(a,cons(b, nil)), a).
%last(cons(a,cons(b, nil)), b).
last(cons(LE, nil), LE).
last(cons(_, T), LE) :- last(T, LE).

%Ex 5.2
%map1(?ListIn, ?ListOut)
%transforms a list of integers into a list where each element is incremented by 1
%examples:
%map1(cons(s(s(zero)), cons(zero, nil)), cons(s(s(s(zero))), cons(s(zero), nil))). -> yes
map1(nil, nil).
map1(cons(H1, T1), cons(s(H1), T2)) :- map1(T1, T2).
%map(?ListIn, ?Inc, ?ListOut)
%transforms a list of integers into a list where each element is incremented by Inc
%examples:
%map(cons(s(s(zero)), cons(zero, nil)), s(s(zero)), cons(s(s(s(s(zero)))), cons(s(s(zero)), nil))). -> yes
map(nil, _, nil).
map(cons(H1, T1), Inc, cons(H2, T2)) :- map(T1, Inc, T2), sum(H1, Inc, H2).

%Ex 5.3
%filter0(@ListIn, -ListOut)
%filter the imput list removing all zeros
%examples:
%filter0(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), cons(s(zero),cons(s(s(zero)),nil))). -> yes
filter0(nil, nil).
filter0(cons(zero, T1), L) :- filter0(T1, L).
filter0(cons(H, T1), cons(H, T2)) :- H \= zero, filter0(T1, T2).
%filter(@L, ?N, ?L)
%filter the imput list removing all element less than N
%examples:
%filter(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), s(zero), cons(s(s(zero)), nil)). -> yes
filter(nil, _, nil).
filter(cons(H, T1), N, L) :- greater_equal(N, H), filter(T1, N, L).
filter(cons(H, T1), N, cons(H, T2)) :- greater(H, N), filter(T1, N, T2).

%Ex 5.4
%count0(@List, -R)
%counts the number of elements greater than zero
%examples:
%count0(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), s(s(zero))). -> yes
count0(L, R) :- filter0(L, LF), size(LF, R).
%count(@List, ?N, ?R)
%counts the number of elements greater than N
%examples:
%count2(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), s(zero), s(zero)). -> yes
count2(L, N, R) :- filter(L, N, LF), size(LF, R).

%Ex 5.5
%find0(@List, -E)
%find first element greather than 0
%examples:
%find0(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), s(s(zero))). -> yes
find0(L, E) :- filter0(L, LF), search(E, LF).
%find(@List, ?N, ?E)
%find first element greather than N
%examples:
%find(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), s(zero), s(s(zero))) -> yes
find(L, N, E) :- filter(L, N, LF), search(E, LF).

%Ex 5.6
%dropRight2(@ListIn, -ListOut)
%removes last 2 elements from list
%examples:
%dropRight2(cons(a, cons(b, cons(c, nil))), cons(a, nil)). -> yes
dropRight2(cons(H, T), cons(H, T2)) :- size(T, ST), greater_equal(ST,s(s(zero))), dropRight2(T, T2).
dropRight2(L, nil):- size(L, SL), SL = s(s(zero)).
%dropRight(@ListIn, ?N, ?ListOut)
%removes last N elements from list
%examples:
%dropRight(cons(a, cons(b, cons(c, nil))), s(zero), cons(a, cons(b, nil))). -> yes
dropRight(cons(H, T), N, cons(H, T2)) :- size(T, ST), greater_equal(ST,N), dropRight(T, N, T2).
dropRight(L, N, nil):- size(L, SL), SL = N.

%Ex 5.7
%dropWhile0(@ListIn, -ListOut)
%examples:
%dropWhile0(cons(s(zero), cons(zero, cons(zero, cons(s(s(s(zero))), nil)))), cons(zero, cons(s(s(s(zero))), nil))). -> yes
dropWhile0(cons(H, T), L) :- greater(H, zero), dropWhile0(T, L).
dropWhile0(cons(H, T), T) :- H = zero.
%dropWhile(@ListIn, ?N, ?ListOut)
%examples:
%dropWhile(cons(s(s(zero)), cons(s(zero), cons(zero, cons(s(s(s(zero))), nil)))), zero, cons(zero, cons(s(s(s(zero))), nil))) -> yes
dropWhile(cons(H, T), N, L) :- greater(H, N), dropWhile(T,N,L).
dropWhile(cons(H, T), N, cons(H, T)) :- greater_equal(N, H).

%Ex 5.8
%partition(?ListIn, ?ListYes, ?ListNo)
%puts the elements greater than 0 in ListYes and the others in ListNo
%examples:
%partition0(cons(zero, cons(s(s(zero)), cons(s(zero), cons(zero, nil)))), cons(s(s(zero)), cons(s(zero), nil)), cons(zero, cons(zero, nil))). -> yes
%littlebug non showa zero quando prende l'elem max 
partition0(nil, nil, nil).
partition0(cons(H, T), L1, cons(H, L2)) :- H = zero, partition0(T, L1, L2).
partition0(cons(H, T), cons(H, L1), L2) :- greater(H, zero), partition0(T, L1, L2).
%partition(?ListIn, ?N, ?ListYes, ?ListNo)
%puts the elements greater than N in ListYes and the others in ListNo
%examples:
%partition(cons(zero, cons(s(s(zero)), cons(s(zero), cons(zero, nil)))), s(zero), cons(s(s(zero)), nil), cons(zero, cons(s(zero), cons(zero, nil)))). -> yes
partition(nil, N, nil, nil).
partition(cons(H, T), N, L1, cons(H, L2)) :- greater_equal(N, H), partition(T, N, L1, L2).
partition(cons(H, T), N, cons(H, L1), L2) :- greater(H, N), partition(T, N, L1, L2).

%Ex 5.9
%reversed(?ListIn, ?ListOut)
%reverses the orders of the ListIn elements
%examples:
%reversed(cons(a, cons(b, cons(c, nil))), cons(c, cons(b, cons(a, nil)))). -> yes
reversed(L, RL) :- reversed(L, nil, RL).
reversed(nil, RL, RL).
reversed(cons(H, T), A, RL) :- reversed(T, cons(H, A), RL).

%Ex 5.10
%drop(?ListIn, ?N, ?ListOut)
%removes first N elements from ListIn
%examples:
%drop(cons(a, cons(b, cons(c, nil))), s(s(zero)), cons(c, nil)). -> yes
drop(L, zero, L).
drop(cons(H, T), s(N), L) :- drop(T, N, L).

%Ex 5.11
%take(?ListIn, ?N, ?ListOut)
%examples:
%take(cons(a, cons(b, cons(d, nil))), s(s(zero)), cons(a, cons(b, nil))). -> yes
take(L, zero, nil).
take(cons(H, T), s(N), cons(H, T2)) :- take(T, N, T2).

%Ex 5.12
%zip(?ListIn1, ?ListIn2, ?ListOut)
%examples:
%zip(cons(a, cons(b, cons(c, nil))), cons(c, cons(b, cons(a, nil))), cons((a,c), cons((b,b), cons((c,a), nil)))). -> yes
zip(nil, nil, nil).
zip(cons(H,T), cons(H2, T2), cons((H,H2),T3)) :- zip(T,T2,T3).


