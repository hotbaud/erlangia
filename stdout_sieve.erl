%%%-------------------------------------------------------------------
%%% @author Anne Marie Merritt
%%% @copyright (C) 2018, Anne Marie Merritt
%%% @doc
%%% This module implements the Sieve of Eratothenes. This version outputs
%%% primes and the Euler Sum of primes to stdout, along with a count of
%%% how many primes were identified. The sum and count do not include '1'.
%%% @end
%%%-------------------------------------------------------------------
-module(stdout_sieve).
-author("Anne Marie Merritt").
-export([main/1, sieve/1, euler_sum/1]).

%main(X) -> io:format("Primes: ~w~n", [sieve(X)]).


main(X) ->
  Primes = sieve(X),
  EulerSum=lists:foldl(fun(E,A)-> E+A end, 0, Primes),
  Primes_len = length(Primes),
  io:format("Euler Sum: ~p PrimeCount: ~p~n", [EulerSum, Primes_len]).


is_toobig(N, L) -> N * N > lists:last(L).

euler_sum(N) -> lists:foldl(fun(E,A)-> E+A end, 0, sieve(N)).

filter(L, Prime) -> lists:reverse(filter(L,Prime,[])).
filter([], _, Acc) -> Acc;
filter([H|T], Prime, Acc) when H rem Prime /= 0 -> filter(T, Prime, [H|Acc]);
filter([_|T], Prime, Acc) -> filter(T, Prime, Acc).

sieve(MaxNum) when MaxNum > 1 -> lists:reverse(sieve(lists:seq(2, MaxNum), []));
sieve(MaxNum) when MaxNum =< 1 -> [].

sieve([], Acc) -> Acc;
sieve([H | T], Acc)->
  case is_toobig(H,T) of
    true ->
      io:format("~p\n", [H]),
      Primes = lists:append([lists:reverse(T), [H], Acc]),
      lists:foreach(fun(N) -> io:format("~p~n",[N]) end, T),
      Primes;
    false->
      io:format("~p\n", [H]),
      sieve(filter(T, H), [H | Acc])
  end.

