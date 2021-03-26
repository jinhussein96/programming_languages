

puzzle_(RowNums,ColNums,Solution) :-
   length(RowNums,NRows),
   length(ColNums,NCols),
   make_rectangle(NRows,NCols,Rows,Cols),
   append(Rows,Cols,Lines),
   append(RowNums,ColNums,LineNums),
   maplist(make_runs,LineNums,LineRuns),
   combine(Lines,LineRuns,LineTasks),
   solve(LineTasks),
   Solution = Rows.
 
combine([],[],[]).
combine([L1|Ls],[N1|Ns],[task(L1,N1)|Ts]) :- combine(Ls,Ns,Ts).

solve([]).
solve([task(Line,LineRuns)|Tasks]) :- 
   place_runs(LineRuns,Line),
   solve(Tasks).


make_rectangle(NRows,NCols,Rows,Cols) :- %bu lazim
   NRows > 0, NCols > 0,
   length(Rows,NRows),
   Pred1 =.. [inv_length, NCols],
   checklist(Pred1,Rows),
   length(Cols,NCols),
   Pred2 =.. [inv_length, NRows],
   checklist(Pred2,Cols),
   unify_rectangle(Rows,Cols).

inv_length(Len,List) :- length(List,Len).

unify_rectangle(_,[]).
unify_rectangle([],_).
unify_rectangle([[X|Row1]|Rows],[[X|Col1]|Cols]) :-
   unify_row(Row1,Cols,ColsR), 
   unify_rectangle(Rows,[Col1|ColsR]).   

unify_row([],[],[]).
unify_row([X|Row],[[X|Col1]|Cols],[Col1|ColsR]) :- unify_row(Row,Cols,ColsR).

make_runs([],[]) :- !.
make_runs([Len1|Lens],[Run1-T|Runs]) :- 
   put_x(Len1,Run1,T),
   make_runs2(Lens,Runs).

make_runs2([],[]).
make_runs2([Len1|Lens],[[' '|Run1]-T|Runs]) :- 
   put_x(Len1,Run1,T),
   make_runs2(Lens,Runs).

put_x(0,T,T) :- !.
put_x(N,['x'|Xs],T) :- N > 0, N1 is N-1, put_x(N1,Xs,T).


place_runs([],[]).
place_runs([Line-Rest|Runs],Line) :- place_runs(Runs,Rest).
place_runs(Runs,[' '|Rest]) :- place_runs(Runs,Rest).
 




% Printing the solution ----------------------------------------------------


print_puzzle_([],ColNums,[]) :- print_colnums(ColNums).
print_puzzle_([RowNums1|RowNums],ColNums,[Row1|Rows]) :-
   print_row(Row1),
   print_rownums(RowNums1),
   print_puzzle_(RowNums,ColNums,Rows).

print_row([]) :- write('  ').
print_row([X|Xs]) :- print_replace(X,Y), write(' '), write(Y), print_row(Xs).
   
print_replace(' ',' ') :- !.
print_replace(x,'*').

print_rownums([]) :- nl.
print_rownums([N|Ns]) :- write(N), write(' '), print_rownums(Ns).

print_colnums(ColNums) :-
   maxlength(ColNums,M,0),
	print_colnums(ColNums,ColNums,1,M).

maxlength([],M,M).
maxlength([L|Ls],M,A) :- length(L,N), B is max(A,N), maxlength(Ls,M,B). 

print_colnums(_,[],M,M) :- !, nl.
print_colnums(ColNums,[],K,M) :- K < M, !, nl,
   K1 is K+1, print_colnums(ColNums,ColNums,K1,M).
print_colnums(ColNums,[Col1|Cols],K,M) :- K =< M, 
   write_kth(K,Col1), print_colnums(ColNums,Cols,K,M).
   
write_kth(K,List) :- nth1(K,List,X), !, writef('%2r',[X]).
write_kth(_,_) :- write('  ').

% --------------------------------------------------------------------------

%%to test the first puzzle ex: test('test1').

test(Name) :- 
   puzzle(Name,Rs,Cs),
   puzzle_(Rs,Cs,Solution), nl,
   print_puzzle_(Rs,Cs,Solution).

puzzle(
	'test1',
	[[3], [2,1], [3,2], [2,2], [6], [1,5], [6], [1], [2]],
	[[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]
	).

puzzle(
	'test2',
	[[3,1], [2,4,1], [1,3,3], [2,4], [3,3,1,3], [3,2,2,1,3],
	 [2,2,2,2,2], [2,1,1,2,1,1], [1,2,1,4], [1,1,2,2], [2,2,8],
	 [2,2,2,4], [1,2,2,1,1,1], [3,3,5,1], [1,1,3,1,1,2],
	 [2,3,1,3,3], [1,3,2,8], [4,3,8], [1,4,2,5], [1,4,2,2],
	 [4,2,5], [5,3,5], [4,1,1], [4,2], [3,3]],
	[[2,3], [3,1,3], [3,2,1,2], [2,4,4], [3,4,2,4,5], [2,5,2,4,6],
	 [1,4,3,4,6,1], [4,3,3,6,2], [4,2,3,6,3], [1,2,4,2,1], [2,2,6],
	 [1,1,6], [2,1,4,2], [4,2,6], [1,1,1,1,4], [2,4,7], [3,5,6],
	 [3,2,4,2], [2,2,2], [6,3]]
	).

puzzle(
	'test3',
	[[5], [2,3,2], [2,5,1], [2,8], [2,5,11], [1,1,2,1,6], [1,2,1,3],
	 [2,1,1], [2,6,2], [15,4], [10,8], [2,1,4,3,6], [17], [17],
	 [18], [1,14], [1,1,14], [5,9], [8], [7]],
	[[5], [3,2], [2,1,2], [1,1,1], [1,1,1], [1,3], [2,2], [1,3,3],
	 [1,3,3,1], [1,7,2], [1,9,1], [1,10], [1,10], [1,3,5], [1,8],
	 [2,1,6], [3,1,7], [4,1,7], [6,1,8], [6,10], [7,10], [1,4,11],
	 [1,2,11], [2,12], [3,13]]
	).
puzzle(
   'test4',
   [[6], [3,1,3], [1,3,1,3], [3,14], [1,1,1], [1,1,2,2], [5,2,2],
    [5,1,1], [5,3,3,3], [8,3,3,3]],
   [[4], [4], [1,5], [3,4], [1,5], [1], [4,1], [2,2,2],
    [3,3], [1,1,2], [2,1,1], [1,1,2], [4,1], [1,1,2], [1,1,1],
    [2,1,2], [1,1,1], [3,4], [2,2,1], [4,1]]
   ).

