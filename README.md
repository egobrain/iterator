# Erlang Iterator: a lazy sequences library.
----------------------------------------------------

## Description ##

Iterator is a function that returns data with next iterator or
done or error message.  
You can use it to organize lazy evolution or backpressure.  

Simple infinite iterator:

```erlang
1> SimpleI = fun Loop() -> {ok, 1, Loop} end.
#Fun<erl_eval.44.90072148>

2> iterator:take(10, SimpleI).
{ok,[1,1,1,1,1,1,1,1,1,1],#Fun<erl_eval.62.90072148>}
```

Complex finite iterator:

```erlang
GetIterator =
1>     (fun Loop(N) ->
1>         fun() ->
1>             case N of
1>                  0 -> done;
1>                  _ when N < 0 -> {error, negative};
1>                  _ -> {ok, N, Loop(N-1)}
1>             end
1>         end
1>     end).
#Fun<erl_eval.30.90072148>

2> iterator:to_list(GetIterator(10)).
{ok,[10,9,8,7,6,5,4,3,2,1]}

3> iterator:to_list(GetIterator(0)).
{ok,[]}

4> iterator:to_list(GetIterator(-1)).
{error,negative}
```

## Usage ##

### Integrate to your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference
to this git repo at your rebar.config.

### Examples ###

```erlang
1> I = iterator:recurrent(fun(A) -> A + 1 end, 0).
#Fun<iterator.13.18590171>

2> iterator:take(10, I).
{ok,[1,2,3,4,5,6,7,8,9,10],#Fun<iterator.13.18590171>}

3> I2 = iterator:map(fun(A) -> A * 2 end, I).
#Fun<iterator.1.18590171>

4> iterator:take(10, I2).
{ok,[2,4,6,8,10,12,14,16,18,20],#Fun<iterator.1.18590171>}

5> IL = iterator:from_list([1,2,5]).
#Fun<iterator.11.18590171>

6> iterator:to_list(IL).
{ok,[1,2,5]}

7> IR = iterator:ciclyc(IL).
#Fun<iterator.16.18590171>

8> iterator:take(10, IR).
{ok,[1,2,5,1,2,5,1,2,5,1],#Fun<iterator.16.18590171>}

9> I3 = iterator:flatmap(fun(A) -> iterator:from_list([-A, A]) end, I2).
#Fun<iterator.4.18590171>

10> iterator:take(10, I3).
{ok,[-2,2,-4,4,-6,6,-8,8,-10,10],#Fun<iterator.8.18590171>}
```

### Warnings !!! ###

With iterator you are working with potentially infinite data.  
Don't use ```to_list/1```, ```foreach/2```, ```fold/3``` functions
until you know that iterator is finite state.  
Use ```dropwhen/1``` before, or ```take/2``` instead.
