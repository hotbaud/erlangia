%%%-------------------------------------------------------------------
%%% @author Anne Marie Merritt
%%% @copyright (C) 2018, Anne Marie Merritt
%%% @doc
%%% This module implements the Sieve of Eratosthenes. This version outputs
%%% primes and the Euler Sum of primes to stdout, along with a count of
%%% how many primes were identified. The sum and count do not include '1'.
%%% @end
%%%-------------------------------------------------------------------
-module(stdout_sieve).
-author("Anne Marie Merritt").
-export([main/1, sieve/1, euler_sum/1]).

main(X) ->
  Primes = sieve(X),

  % Add up all the primes to make a giant pile of prime.
  EulerSum=lists:foldl(fun(E,A)-> E+A end, 0, Primes),

  Primes_len = length(Primes),
  io:format("Euler Sum: ~p PrimeCount: ~p~n", [EulerSum, Primes_len]).

% Separate routine to both compute primes and sum them, for fun.
euler_sum(N) -> lists:foldl(fun(E,A)-> E+A end, 0, sieve(N)).

% The 'filter' routine performs the heavy lifting of removing all multiples
% of the input prime from the remaining array.

filter(L, Prime) -> lists:reverse(filter(L,Prime,[])).
filter([], _, Acc) -> Acc;
filter([H|T], Prime, Acc) when H rem Prime /= 0 -> filter(T, Prime, [H|Acc]);
filter([_|T], Prime, Acc) -> filter(T, Prime, Acc).

% The sieve iterates over the array picking off primes to send to the filter.
% When the square of the input prime is greater than the last (largest) element
% in the array, we're done.

sieve(MaxNum) when MaxNum > 1 -> lists:reverse(sieve(lists:seq(2, MaxNum), []));
sieve(MaxNum) when MaxNum =< 1 -> [].

is_toobig(N, L) -> N * N > lists:last(L).


sieve([], Acc) -> Acc;
sieve([H | T], Acc)->
  case is_toobig(H,T) of
    true ->
      lists:foreach(fun(N) -> io:format("~p~n",[N]) end, [H] ++ T),
      lists:append([lists:reverse(T), [H], Acc]);
    false->
      io:format("~p~n", [H]),
      sieve(filter(T, H), [H | Acc])
  end.
