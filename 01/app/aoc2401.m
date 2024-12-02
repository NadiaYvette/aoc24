:- module aoc2401.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bag.
:- import_module integer.
:- import_module list.
:- import_module pair.
:- import_module string.

main --> read_string_list([], IOResult)
, ( { IOResult = io.ok(LinesUO), copy(LinesUO, Lines) }
    -> ({ {L, R} = lines_to_list_pair(Lines), Sum = list_pair_to_sum(L, R),
          Sim = similarity(bag.from_list(L), bag.from_list(R)) }
          -> io.write_string(io.stdout_stream, integer.to_string(Sum))
           , io.nl
           , io.write_string(io.stdout_stream, integer.to_string(Sim))
    ; io.write_string(io.stdout_stream, "parse error"))
  ; io.write_string(io.stdout_stream, "IO error.")
  )
, nl.

:- func list_pair_to_sum(list(integer), list(integer)) = integer is det.
list_pair_to_sum(Lefts, Rights) = list.foldr((func(X, Y) = X + Y), Diffs, integer.zero)
:- SortedPairs = list.map_corresponding((func(A, B) = {A, B}), Lefts, Rights)
 , Diffs = list.map((func({X, Y}) = integer.abs(X - Y)), SortedPairs).

:- func similarity(bag(integer), bag(integer)) = integer.
:- mode similarity(in, in) = out is det.
similarity(XBag, YBag) = list.foldr((func(X, Y) = X + Y), WS, integer.zero)
:- WS = list.map((func(X-N)=X*integer(N)*integer(count_value(YBag, X))), XS)
 , XS = bag.to_assoc_list(XBag).

:- func lines_to_list_pair(list(string))
            = {list(integer), list(integer)} is semidet.
lines_to_list_pair(Lines) = {list.sort(Lefts), list.sort(Rights)}
:- list.map(read_int_pair, Lines, Pairs)
 , list.map2((pred(C::in, A::out, B::out) is det :- C = {A, B})
            , Pairs
            , Lefts
            , Rights).

:- pred read_string_list(list(string)::in
                       , io.result(list(string))::out
                       , io::di, io::uo) is det.
read_string_list(In, Out) --> io.read_line_as_string(LineResult)
, ( { LineResult = io.ok(Line), list.append(In, [Line], Mid) }
      , read_string_list(Mid, Out)
  ; { LineResult = io.eof }
      , { Out = io.ok(In) }
  ; { LineResult = io.error(E) }
      , { Out = io.error(E) }
).

% list.map/2 doesn't have a mode for a semidet function arg.
:- func read_int_pair(string) = {integer, integer} is semidet.
read_int_pair(S) = {N, K} :- read_int_pair(S, {N, K}).
:- pred read_int_pair(string::in, {integer, integer}::out) is semidet.
read_int_pair(S, {N, K}) :-
  integer.from_string(W1, N),
  integer.from_string(W2, K),
  [W1, W2] = string.words(S).
