:- module aoc2402.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module string.

main --> read_lines([], Result)
, ( { Result = io.error(_) }
        , io.write_string(io.stdout_stream, "IO error"), io.nl
  ; { Result = io.eof }
        , io.write_string(io.stdout_stream, "EOF"), io.nl
  ; { Result = io.ok(Lines) }
        , handle_lines(Lines)
        % , ({ NSS = cvt(Lines)
          % , Safes = list.filter((pred(XS::in) is semidet :- issafe(XS)), NSS)
          % , Unsafes = list.filter((pred(XS::in) is semidet :- not issafe(XS)), NSS) }
        % , io.write_string(io.stdout_stream, string.from_int(list.length(Safes)))
        % , io.write_string(io.stdout_stream, " Safe lists")
        % , io.nl
        % , io.write_string(io.stdout_stream, string.from_int(list.length(Unsafes)))
        % , io.write_string(io.stdout_stream, " Unsafe lists"), io.nl
      % ; io.write_string(io.stdout_stream, "parse failure"), io.nl)
  ).

:- pred handle_lines(list(string)::in, io::di, io::uo) is det.
handle_lines(Lines) -->
( { NSS = cvt(Lines)
  , Safes = list.filter(issafe, NSS)
  , SafeLen = list.length(Safes)
  , Unsafes = list.filter((pred(XS::in) is semidet :- not issafe(XS)), NSS)
  , UnsafeLen = list.length(Unsafes)
  , SafeExtras = list.filter(issafe_extra, NSS)
  , SafeExtraLen = list.length(SafeExtras)
  , UnsafeExtras = list.filter((pred(XS::in) is semidet :- not issafe_extra(XS)), NSS)
  , UnsafeExtraLen = list.length(UnsafeExtras) }
  -> io.write_string(io.stdout_stream, string.from_int(SafeLen))
  , io.write_string(io.stdout_stream, " Safe lists")
  , io.nl
  , io.write_string(io.stdout_stream, string.from_int(UnsafeLen))
  , io.write_string(io.stdout_stream, " Unsafe lists")
  , io.nl
  , io.write_string(io.stdout_stream, string.from_int(SafeExtraLen))
  , io.write_string(io.stdout_stream, " Extra Safe lists")
  , io.nl
  , io.write_string(io.stdout_stream, string.from_int(UnsafeExtraLen))
  , io.write_string(io.stdout_stream, " Extra Unsafe lists")
  , io.nl
; io.write_string(io.stdout_stream, "parse failure")
  , io.nl).

:- func cvt(list(string)) = list(list(integer)) is semidet.
:- pred cvt(list(string)::in, list(list(integer))::out) is semidet.
cvt(SS) = NSS :- list.map(cvtOne, SS, NSS).
cvt(SS, NSS) :- list.map(cvtOne, SS, NSS).

:- func cvtOne(string) = list(integer) is semidet.
:- pred cvtOne(string::in, list(integer)::out) is semidet.
cvtOne(S) = NS
:- list.map(integer.from_string, WS, NS)
 , WS = string.words(S).
cvtOne(S, NS)
:- list.map(integer.from_string, WS, NS)
 , WS = string.words(S).

:- pred issafe_extra(list(integer)::in) is semidet.
issafe_extra(NS1) :- list.delete(NS1, _N, NS2), issafe(NS2).

:- pred issafe(list(integer)::in) is semidet.
issafe([H1 | T @ [H2 | _]]) :-
( H1 < H2, issafe_inc([H1|T])
; H1 > H2, issafe_dec([H1|T])).

:- pred issafe_inc(list(integer)::in) is semidet.
issafe_inc([H1 | T @ [H2 | _]]) :- H1 < H2, H2 - H1 =< integer(3),
  (T = [_|[_|_]] -> issafe_inc(T) ; true).

:- pred issafe_dec(list(integer)::in) is semidet.
issafe_dec([H1 | T @ [H2 | _]]) :- H1 > H2, H1 - H2 =< integer(3),
  (T = [_|[_|_]] -> issafe_dec(T) ; true).

:- pred read_lines(list(string)::in, io.result(list(string))::out, io::di, io::uo) is det.
read_lines(SI, SO) --> read_line_as_string(io.stdin_stream, Result)
, ( { Result = io.error(E) }
        , { SO = io.error(E) }
  ; { Result = io.eof }
        , { SO = io.ok(SI) }
  ; { Result = io.ok(Line) }
        , { list.append(SI, [Line], OutLines) }
           , read_lines(OutLines, SO)
  ).
