-module(iterator).

-export([
         map/2,
         filter/2,
         filtermap/2,
         flatmap/2,
         over/3,
         dropwhen/2,
         dropwhile/2,

         append/2,
         ciclyc/1,

         empty/0,
         recurrent/2,

         foreach/2,
         fold/3,
         take/2,
         takewhile/2,

         from_list/1,
         to_list/1
        ]).

-type iterator(A, R) :: fun(() -> {ok, A, iterator(A, R)} | done | {error, R}).
-type iterator(A) :: iterator(A, any()).
-type iterator() :: iterator(any(), any()).

-export_type([
              iterator/2,
              iterator/1,
              iterator/0
             ]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec empty() -> iterator().
empty() -> fun() -> done end.

-spec map(fun((A) -> B), iterator(A, R)) -> iterator(B, R).
map(Fun, Iterator) ->
    fun() ->
        case Iterator() of
            {ok, Data, Next} ->
                {ok, Fun(Data), map(Fun, Next)};
            done -> done;
            {error, _Reason} = Err -> Err
        end
    end.

-spec foreach(fun((A) -> ok), iterator(A, R)) -> ok | {error, R}.
foreach(Fun, Iterator) ->
    case Iterator() of
        {ok, Data, Next} ->
            _ = Fun(Data),
            foreach(Fun, Next);
        done -> ok;
        {error, _Reason} = Err -> Err
    end.

-spec filter(fun((A) -> boolean()), iterator(A, R)) -> iterator(A, R).
filter(Fun, Iterator) ->
    fun() ->
        (fun Loop(I) ->
             case I() of
                 {ok, Data, Next} ->
                     case Fun(Data) of
                         true -> {ok, Data, filter(Fun, Next)};
                         false -> Loop(Next)
                     end;
                 done -> done;
                 {error, _Reason} = Err -> Err
             end
         end)(Iterator)
    end.

-spec filtermap(fun((A) -> {true, B} | false), iterator(A, R)) -> iterator(B, R).
filtermap(Fun, Iterator) ->
    fun() ->
        (fun Loop(I) ->
             case I() of
                 {ok, Data, Next} ->
                     case Fun(Data) of
                         {true, Data2} -> {ok, Data2, filtermap(Fun, Next)};
                         false -> Loop(Next)
                     end;
                 done -> done;
                 {error, _Reason} = Err -> Err
             end
         end)(Iterator)
    end.

-spec fold(fun((A, S) -> S), S, iterator(A, R)) -> {ok, S} | {error, R}.
fold(Fun, State, Iterator) ->
    case Iterator() of
        {ok, Data, Next} ->
            fold(Fun, Fun(Data, State), Next);
        done -> {ok, State};
        {error, _Reason} = Err -> Err
    end.

-spec flatmap(fun((A) -> iterator(B, R2)), iterator(A, R1)) -> iterator(B, R1|R2).
flatmap(Fun, Iterator) ->
    fun() ->
        case Iterator() of
            {ok, Data, Next} ->
                (append(Fun(Data), flatmap(Fun, Next)))();
            done -> done;
            {error, _Reason} = Err -> Err
        end
    end.

-spec over(fun((A,S) -> {B, S}), S, iterator(A, R)) -> iterator(B, R).
over(Fun, S, Iterator) ->
    fun() ->
        case Iterator() of
            {ok, Data, Next} ->
                {Value, S2} = Fun(Data, S),
                {ok, Value, over(Fun, S2, Next)};
            done -> done;
            {error, _Reason} = Err -> Err
        end
    end.

-spec dropwhen(fun((A) -> boolean()), iterator(A, R)) -> iterator(A, R).
dropwhen(Fun, Iterator) ->
    fun() ->
        case Iterator() of
            {ok, Data, Next} ->
                case Fun(Data) of
                    true -> done;
                    false -> {ok, Data, dropwhen(Fun, Next)}
                end;
            done -> done;
            {error, _Reason} = Err -> Err
        end
    end.

-spec dropwhile(fun((A) -> boolean()), iterator(A, R)) -> iterator(A, R).
dropwhile(Fun, Iterator) ->
    fun() ->
        (fun Loop(I) ->
             case I() of
                 {ok, Data, Next}=R ->
                     case Fun(Data) of
                         true -> Loop(Next);
                         false -> R
                     end;
                 done -> done;
                 {error, _Reason} = Err -> Err
             end
         end)(Iterator)
    end.


-spec append(iterator(A, R1), iterator(B, R2)) -> iterator(A|B, R1|R2).
append(Iterator1, Iterator2) ->
    fun() ->
        case Iterator1() of
            {ok, Data, Next} -> {ok, Data, append(Next, Iterator2)};
            done -> Iterator2();
            {error, _Reason} = Err -> Err
        end
    end.

-spec ciclyc(iterator(A, R)) -> iterator(A, R).
ciclyc(Iterator) ->
    (fun Loop(I) ->
        fun() ->
            case I() of
                {ok, Data, Next} -> {ok, Data, Loop(Next)};
                done -> (ciclyc(Iterator))();
                {error, _Reason} = Err -> Err
            end
        end
    end)(Iterator).

-spec from_list([A]) -> iterator(A, any()).
from_list(List) ->
    Loop =
        fun([]) -> done;
           ([H|T]) -> {ok, H, from_list(T)}
        end,
    fun() -> Loop(List) end.

-spec to_list(iterator(A, R)) -> {ok, [A]} | {error, R}.
to_list(Iterator) ->
    case fold(fun(H, T) -> [H|T] end, [], Iterator) of
        {ok, L} -> {ok, lists:reverse(L)};
        {error, _Reason} = Err -> Err
    end.

-spec recurrent(fun((A) -> A), A) -> iterator(A).
recurrent(Fun, S) ->
    fun() ->
        Next = Fun(S),
        {ok, Next, recurrent(Fun, Next)}
    end.

-spec take(N :: pos_integer(), iterator(A, R)) ->
        {ok, [A], iterator(A, R)} | {error, R}.
take(N, Iterator) when N > 0 ->
    take_(N, [], Iterator).
take_(0, Acc, I) -> {ok, lists:reverse(Acc), I};
take_(C, Acc, I) ->
    case I() of
        {ok, Data, Next} -> take_(C-1, [Data|Acc], Next);
        done -> {ok, lists:reverse(Acc), empty()};
        {error, _Reason} = Err -> Err
    end.

-spec takewhile(fun((A) -> boolean()), iterator(A, R)) ->
        {ok, [A], iterator(A, R)} | { error, R}.
takewhile(Fun, Iterator) ->
    takewhile_(Fun, [], Iterator).
takewhile_(Fun, Acc, I) ->
    case I() of
        {ok, Data, Next} = R ->
            case Fun(Data) of
                true -> takewhile_(Fun, [Data|Acc], Next);
                false ->
                    {ok, lists:reverse(Acc), fun() -> R end}
            end;
        done -> {ok, lists:reverse(Acc), empty()};
        {error, _Reason} = Err -> Err
    end.

%% =============================================================================
%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_list_test_() ->
    [
     {<<"empty">>,
      fun() ->
          I = from_list([]),
          done = I()
      end},
     {<<"one">>,
      fun() ->
          I = from_list([1]),
          {ok, 1, I2} = I(),
          done = I2()
      end},
     {<<"many">>,
      fun() ->
          List = lists:seq(1, 100),
          LastI =
              lists:foldl(
                  fun(E, I) ->
                      {ok, E, I2} = I(),
                      I2
                  end, from_list(List), List),
          done = LastI()
      end}
    ].

to_list_test() ->
    I0 = empty(),
    I1 = fun() -> {ok, 1, I0} end,
    I2 = fun() -> {ok, 2, I1} end,
    I3 = fun() -> {ok, 3, I2} end,
    ?assertEqual({ok, []}, to_list(I0)),
    ?assertEqual({ok, [1]}, to_list(I1)),
    ?assertEqual({ok, [2, 1]}, to_list(I2)),
    ?assertEqual({ok, [3, 2, 1]}, to_list(I3)).

map_test_() ->
    Tests =
        [
         {[], []},
         {[1], [2]},
         {[1,2,3,4], [2,4,6,8]}
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          I = from_list(D),
          MI = map(fun(A) -> A*2 end, I),
          {ok, R} = to_list(MI)
      end} || {D, R} <- Tests
    ].

foreach_test_() ->
    K = '$foreach_test_',
    Tests =
        [
         [],
         [1],
         [1,2,3,4]
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          put(K, []),
          ok = foreach(fun(E) -> put(K, [E|get(K)]) end, from_list(D)),
          D = lists:reverse(get(K))
      end} || D <- Tests
    ].

filter_test_() ->
    Tests =
        [
         {[], []},
         {[1], []},
         {[1,2,3,4], [2,4]}
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          I = from_list(D),
          MI = filter(fun(A) -> A rem 2 =:= 0 end, I),
          {ok, R} = to_list(MI)
      end} || {D, R} <- Tests
    ].

fold_test_() ->
    Tests =
        [
         [],
         [1],
         [1,2,3,4]
        ],
    [
     {iolist_to_binary(io_lib:format("~p", [D])),
      fun() ->
          I = from_list(D),
          {ok, R} = fold(fun(E, A) -> [E|A] end, [], I),
          D = lists:reverse(R)
      end} || D <- Tests
    ].

append_test_() ->
    Tests =
        [
         {[1], [2], [1,2]},
         {[], [1,2,3], [1,2,3]},
         {[1,2,3], [], [1,2,3]}
        ],
    [{<<"empty">>, fun() -> done = (append(empty(), empty()))() end} |
     [
      {iolist_to_binary(io_lib:format("~p + ~p = ~p", [A, B, R])),
       fun() ->
           {ok, R, F} = take(length(R), append(from_list(A), from_list(B))),
           done = F()
       end} || {A, B, R} <- Tests
     ]].

take_test() ->
    List = lists:seq(1, 10),
    I = from_list(List),
    {ok, [1], _} = take(1, I),
    {ok, [1,2], _} = take(2, I),
    {ok, _, LastI} = take(100, I),
    done = LastI().

take_while_test() ->
    List = lists:seq(1, 10),
    I = from_list(List),
    {ok, [1,2,3,4,5], LastI} = iterator:takewhile(fun(A) -> A < 6 end, I),
    {ok, [6,7,8,9,10]} = to_list(LastI).

over_test() ->
    ?assertEqual(
         {ok, [0,1,3,6,10]},
         to_list(
             over(fun(A, B) -> {B, A+B} end, 0,
                  iterator:from_list(lists:seq(1,5))))).

recurrent_test() ->
    {ok, Result, _} = take(6, recurrent(fun(A) -> A + 1 end, 0)),
    ?assertEqual([1,2,3,4,5,6], Result).

dropwhen_test() ->
    List = lists:seq(1, 10),
    I = from_list(List),
    I2 = dropwhen(fun(A) -> A > 5 end, I),
    {ok, [1,2,3,4,5]} = to_list(I2).

dropwhile_test() ->
    List = lists:seq(1, 10),
    I = from_list(List),
    I2 = dropwhile(fun(A) -> A =< 5 end, I),
    {ok, [6,7,8,9,10]} = to_list(I2).

-endif.
