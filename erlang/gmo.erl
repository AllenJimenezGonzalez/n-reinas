-module(gmo).
-export([extraQueens/1]).

%%Domain: 2 Tuples of Integers
%%Codomain: Boleans, if there is a same value in X, its false
validColumn({X,_Y1},{X,_Y2}) -> false;
validColumn({_X1,_Y1},{_X2,_Y2}) -> true.

%%Domain: 2 Tuples of Integers%
%%Codomain: Boleans, if there is a same value in Y, its false
validRow({_X1,Y},{_X2,Y}) -> false;
validRow({_X1,_Y1},{_X2,_Y2}) -> true.

%%Domain: 2 Tuples of Integers
%%Codomain: Boleans
validDiag({X1,Y1},{X2,Y2}) when (X1-X2=< Y1-Y2) andalso X1-X2 =:= 0  -> false;
validDiag({X1,Y1},{X2,Y2}) when (X1-X2> Y1-Y2) andalso Y1-Y2 =:= 0  -> false;
validDiag({X1,Y1},{X2,Y2}) -> abs((Y1-Y2)/ (X1-X2)) =/= 1.0.
 
fullValid({X1,Y1},{X2,Y2}) ->   validRow({X1,Y1},{X2,Y2})  andalso validColumn({X1,Y1},{X2,Y2}) andalso validDiag({X1,Y1},{X2,Y2}).

bactracking({_X,_Y}, []) -> true;
bactracking({X1,Y1}, [{X2,Y2}| T]) -> fullValid({X1, Y1}, {X2, Y2}) andalso bactracking({X1, Y1}, T).

makeL(N1, N2) when N1 =:= N2 -> [N2];
makeL(N1, N2) when N1<N2 -> [N1| makeL (N1+1, N2)].

fullQueens(0, _Row) -> [[]];
%%fullQueens(0, _Row, _Cant)-> [[]];
fullQueens(N1, Row, Cant) -> [[{N1, N2}| T] ||T <- fullQueens((N1-1), Row), N2 <- makeL(1, Row), bactracking({N1, N2}, T)].

extraQueens(N) -> fullQueens(N, N), length(fullQueens(N, N)).



