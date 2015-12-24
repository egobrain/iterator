-module(zlist).

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

-type zlist(A) :: fun(() -> maybe_improper_list(A, zlist(A))) | empty_zlist().
-type empty_zlist() :: fun(() -> []).

-export_type([
              zlist/1,
              empty_zlist/0
             ]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec empty() -> empty_zlist().
empty() -> fun() -> [] end.

-spec map(fun((A) -> B), zlist(A)) -> zlist(B).
map(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                [Fun(Data)|map(Fun, Next)];
            Done -> Done
        end
    end.

-spec foreach(fun((A) -> ok), zlist(A)) -> ok.
foreach(Fun, Zlist) ->
    case Zlist() of
        [Data|Next] ->
            _ = Fun(Data),
            foreach(Fun, Next);
        _Done -> ok
    end.

-spec filter(fun((A) -> boolean()), zlist(A)) -> zlist(A).
filter(Fun, Zlist) ->
    fun() ->
        (fun Loop(Z) ->
             case Z() of
                 [Data|Next] ->
                     case Fun(Data) of
                         true -> [Data|filter(Fun, Next)];
                         false -> Loop(Next)
                     end;
                 Done -> Done
             end
         end)(Zlist)
    end.

-spec filtermap(fun((A) -> {true, B} | false), zlist(A)) -> zlist(B).
filtermap(Fun, Zlist) ->
    fun() ->
        (fun Loop(Z) ->
             case Z() of
                 [Data|Next] ->
                     case Fun(Data) of
                         {true, Data2} -> [Data2|filtermap(Fun, Next)];
                         false -> Loop(Next)
                     end;
                 Done -> Done
             end
         end)(Zlist)
    end.

-spec fold(fun((A, S) -> S), S, zlist(A)) -> S.
fold(Fun, State, Zlist) ->
    case Zlist() of
        [Data|Next] ->
            fold(Fun, Fun(Data, State), Next);
        _Done -> State
    end.

-spec flatmap(fun((A) -> zlist(B)), zlist(A)) -> zlist(B).
flatmap(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                (append(Fun(Data), flatmap(Fun, Next)))();
            Done -> Done
        end
    end.

-spec over(fun((A,S) -> {B, S}), S, zlist(A)) -> zlist(B).
over(Fun, S, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                {Value, S2} = Fun(Data, S),
                [Value|over(Fun, S2, Next)];
            Done -> Done
        end
    end.

-spec dropwhen(fun((A) -> boolean()), zlist(A)) -> zlist(A).
dropwhen(Fun, Zlist) ->
    fun() ->
        case Zlist() of
            [Data|Next] ->
                case Fun(Data) of
                    true -> [];
                    false -> [Data|dropwhen(Fun, Next)]
                end;
            Done -> Done
        end
    end.

-spec dropwhile(fun((A) -> boolean()), zlist(A)) -> zlist(A).
dropwhile(Fun, Zlist) ->
    fun() ->
        (fun Loop(Z) ->
             case Z() of
                 [Data|Next]=R ->
                     case Fun(Data) of
                         true -> Loop(Next);
                         false -> R
                     end;
                 Done -> Done

             end
         end)(Zlist)
    end.

-spec append(zlist(A), zlist(B)) -> zlist(A|B).
append(Zlist1, Zlist2) ->
    fun() ->
        case Zlist1() of
            [Data|Next] -> [Data|append(Next, Zlist2)];
            _Done -> Zlist2()
        end
    end.

-spec ciclyc(zlist(A)) -> zlist(A).
ciclyc(Zlist) ->
    (fun Loop(Z) ->
        fun() ->
            case Z() of
                [Data|Next] -> {ok, Data, Loop(Next)};
                _Done -> (ciclyc(Zlist))()
            end
        end
    end)(Zlist).

-spec from_list([A]) -> zlist(A).
from_list(List) ->
    fun() ->
        case List of
            [H|T] -> [H|from_list(T)];
            _ -> List
        end
    end.

-spec to_list(zlist(A)) -> [A].
to_list(Zlist) -> lists:reverse(fold(fun(H, T) -> [H|T] end, [], Zlist)).

-spec recurrent(fun((A) -> A), A) -> zlist(A).
recurrent(Fun, S) ->
    fun() ->
        Next = Fun(S),
        [Next|recurrent(Fun, Next)]
    end.

-spec take(N :: pos_integer(), zlist(A)) -> {[A], zlist(A)}.
take(N, Zlist) when N > 0 ->
    take_(N, [], Zlist).
take_(0, Acc, Z) -> {lists:reverse(Acc), Z};
take_(C, Acc, Z) ->
    case Z() of
        [Data|Next] -> take_(C-1, [Data|Acc], Next);
        _Done -> {lists:reverse(Acc), empty()}
    end.

-spec takewhile(fun((A) -> boolean()), zlist(A)) -> {[A], zlist(A)}.
takewhile(Fun, Zlist) -> takewhile_(Fun, [], Zlist).
takewhile_(Fun, Acc, Z) ->
    case Z() of
        [Data|Next] = R ->
            case Fun(Data) of
                true -> takewhile_(Fun, [Data|Acc], Next);
                false -> {lists:reverse(Acc), fun() -> R end}
            end;
        _Done -> {lists:reverse(Acc), empty()}
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
          Z = from_list([]),
          [] = Z()
      end},
     {<<"one">>,
      fun() ->
          Z = from_list([1]),
          [1|Z2] = Z(),
          [] = Z2()
      end},
     {<<"many">>,
      fun() ->
          List = lists:seq(1, 100),
          LastZ = lists:foldl(
              fun(E, Z) -> [E|Z2] = Z(), Z2 end,
              from_list(List),
              List),
          [] = LastZ()
      end}
    ].

to_list_test() ->
    Z0 = empty(),
    Z1 = fun() -> [1|Z0] end,
    Z2 = fun() -> [2|Z1] end,
    Z3 = fun() -> [3|Z2] end,
    ?assertEqual([], to_list(Z0)),
    ?assertEqual([1], to_list(Z1)),
    ?assertEqual([2, 1], to_list(Z2)),
    ?assertEqual([3, 2, 1], to_list(Z3)).

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
          Z = from_list(D),
          MZ = map(fun(A) -> A*2 end, Z),
          R = to_list(MZ)
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
          Z = from_list(D),
          MZ = filter(fun(A) -> A rem 2 =:= 0 end, Z),
          R = to_list(MZ)
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
          Z = from_list(D),
          R = fold(fun(E, A) -> [E|A] end, [], Z),
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
    [{<<"empty">>, fun() -> [] = (append(empty(), empty()))() end} |
     [
      {iolist_to_binary(io_lib:format("~p + ~p = ~p", [A, B, R])),
       fun() ->
           {R, F} = take(length(R), append(from_list(A), from_list(B))),
           [] = F()
       end} || {A, B, R} <- Tests
     ]].

take_test() ->
    List = lists:seq(1, 10),
    Z = from_list(List),
    {[1], _} = take(1, Z),
    {[1,2], _} = take(2, Z),
    {_, LastZ} = take(100, Z),
    [] = LastZ().

take_while_test() ->
    List = lists:seq(1, 10),
    Z = from_list(List),
    {[1,2,3,4,5], LastZ} = zlist:takewhile(fun(A) -> A < 6 end, Z),
    [6,7,8,9,10] = to_list(LastZ).

over_test() ->
    ?assertEqual(
         [0,1,3,6,10],
         to_list(
             over(fun(A, B) -> {B, A+B} end, 0,
                  from_list(lists:seq(1,5))))).

recurrent_test() ->
    {Result, _} = take(6, recurrent(fun(A) -> A + 1 end, 0)),
    ?assertEqual([1,2,3,4,5,6], Result).

dropwhen_test() ->
    List = lists:seq(1, 10),
    Z = from_list(List),
    Z2 = dropwhen(fun(A) -> A > 5 end, Z),
    [1,2,3,4,5] = to_list(Z2).

dropwhile_test() ->
    List = lists:seq(1, 10),
    Z = from_list(List),
    Z2 = dropwhile(fun(A) -> A =< 5 end, Z),
    [6,7,8,9,10] = to_list(Z2).

-endif.
