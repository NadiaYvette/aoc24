:- module aoc2401.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module integer.
:- import_module list.
:- import_module string.

main --> read_string_list([], IOResult)
, ( { IOResult = io.ok(LinesUO), copy(LinesUO, Lines) }
    -> ({ Sum = lines_to_sum(Lines) }
          -> io.write_string(io.stdout_stream, integer.to_string(Sum))
    ; io.write_string(io.stdout_stream, "parse error"))
  ; io.write_string(io.stdout_stream, "IO error.")
  )
, nl.

:- func lines_to_sum(list(string)) = integer is semidet.
lines_to_sum(Lines) = list.foldr((func(X, Y) = X + Y), Diffs, integer.zero)
:- list.map(read_int_pair, Lines, Pairs)
 , list.map2((pred(C::in, A::out, B::out) is det :- C = {A, B})
            , Pairs
            , Lefts
            , Rights)
 , SortedPairs = list.map_corresponding((func(A, B) = {A, B})
                                       , list.sort(Lefts)
                                       , list.sort(Rights))
 , Diffs = list.map((func({X, Y}) = integer.abs(X - Y)), SortedPairs).

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
