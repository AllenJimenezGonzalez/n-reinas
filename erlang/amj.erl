%%Erlang Project.
%%Students: Méndez Gustavo, Jiménez Allen, Alvarado Daniela.
-module(amj).
-export([backtrackingNReinas/2]).

%%Domain: 2 tuples of integers.%%
%%Codomain: Boleans, if there is a same value in X, its false.%%
validColumn({X,_Y1},{X,_Y2}) -> false;
validColumn({_X1,_Y1},{_X2,_Y2}) -> true.

%%Domain: Two tuples of integers.%
%%Codomain: Boleans, if there is a same value in Y, its false.%%
validRow({_X1,Y},{_X2,Y}) -> false;
validRow({_X1,_Y1},{_X2,_Y2}) -> true.

%%Domain: Two tuples of integers.%%
%%Codomain: Boleans, with a mathematical formule, we can know when two points are in the same diagonal ((Y1-Y2)/ (X1-X2))= 1, 
%%when is 1 its false (cause are in the same diagonal) and when not, its true.%%
%%Another thing that have, when the divisor is 0 its false, because we can't divide by 0 and the result is not 1.%%
validDiag({X1,Y1},{X2,Y2}) when (X1-X2=< Y1-Y2) andalso X1-X2 =:= 0  -> false;
validDiag({X1,Y1},{X2,Y2}) when (X1-X2> Y1-Y2) andalso Y1-Y2 =:= 0  -> false;
validDiag({X1,Y1},{X2,Y2}) -> abs((Y1-Y2)/ (X1-X2)) =/= 1.0.
 
%%Domain: 2 tuples of integers.%%
%%Codomain: Boleans, here we combine all the previous methods to verify if the queen can be placed in two spaces.%%
fullValid({X1,Y1},{X2,Y2}) ->   validRow({X1,Y1},{X2,Y2})  andalso validColumn({X1,Y1},{X2,Y2}) andalso validDiag({X1,Y1},{X2,Y2}).

%%Domain: A tuple of Integers and a list.%%
%%Codomain: Boleans, now in this method we apply backtraking to do the previous method with a list of spaces.%%
bactracking({_X,_Y}, []) -> true;
bactracking({X1,Y1}, [{X2,Y2}| T]) -> fullValid({X1, Y1}, {X2, Y2}) andalso bactracking({X1, Y1}, T).

%%Domain: Two integers.%%
%%Codomain: A list with the elements between the 2 integers.%%
makeL(N1, N2) when N1 =:= N2 -> [N2];
makeL(N1, N2) when N1<N2 -> [N1| makeL (N1+1, N2)].

%%Domain: Two same integers.%%
%%Codomain: A list with the all the possible combinations of the n queens problem, with the integers the funtion call himself 
%%and the backtraking method for every element that makeL do starting with 1 and ending in the row.%%
fullQueens(0, _Row) -> [[]];
fullQueens(N1, Row) -> [[{N1, N2}| T] ||T <- fullQueens(N1-1, Row), N2 <- makeL(1, Row), bactracking({N1, N2}, T)].

%%Domain: A integer and a list.%%
%%Codomain: The firts part of the split list in the integer.%%
getList(N, L) when length(L) =< N -> L ;
getList(N, L)-> {L1, _L2} = lists:split(N, L), L1.

%%Domain: Two integers, the firts number have to be bigger than 3 and is the number of queens and the size of the table(NxN) and the second one is the number of solutions that the user wants.%%
%%Codomain: A list of results of the n queens problem with the number of solutions that the user wanted.%%
backtrackingNReinas(N, Cant) ->  getList(Cant ,fullQueens(N, N)).