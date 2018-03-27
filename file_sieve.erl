%%%-------------------------------------------------------------------
%%% @author Anne Marie Merritt
%%% @copyright (C) 2018 Anne Marie Merritt
%%% @doc
%%% This program uses the Sieve of Eratosthenes to find primes between
%%% 2 and the given maximum. The default output is to a file "primes.txt",
%%% otherwise a different file can be specified in the second argument.
%%% @end
%%% Created : 24. Mar 2018 9:58 PM
%%%-------------------------------------------------------------------
-module(file_sieve).
-author("Anne Marie Merritt").
-export([main/1, main/2]).

main(X) -> main(X, "primes.txt").

main(X, FileName) ->
  {ok, FileDesc} = file:open(FileName, write),
  Primes = sieve(X, FileDesc),
  EulerSum=lists:foldl(fun(E,A)-> E+A end, 0, Primes),
  io:fwrite(FileDesc, "Euler Sum: ~p PrimeCount: ~p~n", [EulerSum, length(Primes)]),
  file:close(FileDesc).

filter(L, Prime) -> lists:reverse(filter(L,Prime,[])).
filter([], _, Acc) -> Acc;
filter([H|T], Prime, Acc) when H rem Prime /= 0 -> filter(T, Prime, [H|Acc]);
filter([_|T], Prime, Acc) -> filter(T, Prime, Acc).

sieve(MaxNum, FileDesc) when MaxNum > 1 -> 
  lists:reverse(sieve((lists:seq(2, MaxNum)),[], FileDesc));
sieve(MaxNum, _) when MaxNum =< 1 -> [].

% If the square of the candidate is larger then the last element, we can stop
% searching since the remaining elements in the list will be prime.
is_toobig(N, L) -> N * N > lists:last(L).

sieve([], Acc, _) -> Acc;
sieve([H | T], Acc, FileDesc)->
  case is_toobig(H,T) of
    true ->
      lists:foreach(fun(N) -> io:fwrite(FileDesc,"~p~n",[N]) end, [H] ++ T),
      lists:append([lists:reverse(T), [H], Acc]);
    false->
      io:fwrite(FileDesc, "~p~n", [H]),
      sieve(filter(T, H), [H | Acc], FileDesc)
  end.

