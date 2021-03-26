:-style_check(-singleton).

%-----------
%---PART4---
%-----------
element(E,S) :- member(E,S).

union2(S1,S2,S3) :- union1(S1,S2,X),equivalent(X,S3).
union1([],L,L).
union1([H | T], L2, I) :- element(L2, H),!, union1(T,L2, I).
union1([H | T], L2, [H | I]) :- union1(T, L2,I).


equivalent(S1, S2) :- equivalent2(S1,S2), equivalent2(S2,S1).
equivalent2([],_).
equivalent2([E|S1],S2):- element(E,S2), equivalent2(S1,S2).



intersect(S1,S2,S3) :- intersection(S1,S2,X) , equivalent(X,S3).

intersection([X|Y] , M , [X|Z]) :- 
			element(X,M),intersection(Y,M,Z).
intersection([X|Y] , M , Z) :-
			\+ element(X,M),intersection(Y,M,Z).
intersection([],M,[]).


