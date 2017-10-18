divide(L, N, [[], L]) :- N < 2. 
divide([X|Xs], N, [L1, L2]) :- 
	N1 is N - 2, 
	divide(Xs, N1, [T1, T2]), 
	L1 = [X|T1], 
	L2 = T2. 