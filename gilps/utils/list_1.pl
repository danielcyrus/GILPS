%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Author: Jose Santos <jcas81@gmail.com>
% Date: 2009-01-28
%
%     This file contains misc. list utility predicates not present in library(lists)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(utils_list,
            [
              afterFirstNElemsOf/3,
              member_identical/2,
              member_identical_chk/2,
              member_identical_pos/3,
              length_up_to/3,
              createList/3,
              numbersList/3,
              firstNElemsOf/3,
              find_minimum/3,
              replaceAt/4,
              splitAtPos/4,
              split/4,
              elemsAtPos/3,
              order_list/2,
              custom_qsort/3,
              relative_position/2,
              random_elem/2,
              random_elem/3,
              random_elem/4,
              randomMember/2,
              randomMember/3,
              randomPairs/3,
              randomPermutation/2,
              remove_after/3,
              remove_positions/3,
              n_randomElems/3,
              tuples_to_lists/2,
              lists_to_tuples/3,
              merge_keys/2,
              merge_sorted_keys/2,
              append_keys/2,            % these are similar to merge_keys but do not sort the values, just append them
              append_sorted_keys/2
            ]
         ).

% YAP modules
:- use_module(library(lists), [member/2, append/3, nth/4, nth/3]).
:- use_module(library(ordsets), [ord_subset/2]).
:- use_module(library(apply_macros), [maplist/3]).
:- use_module(library(random), [random/3]).


%:- use_module('../type_checker/type_check_disabler').

%:- srandom(7). % set random seed to an arbitrary constant, % this is now in settings

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% member_identical(+Elem, +List)
% member_identical_chk(+Elem, +List)
% member_identical_pos(+Elem, +List, -Pos)
%
% Given:
%   Elem: the element to check for identicality in List (likely a variable)
%   List: a list of of elements (likely a list of variables)
%
% member_identical/2: Succeeds once for each element in List identical to Elem
% member_identica_chk/2: Succeeds at the first occurrences of Elem in List, as efficiently as possible
%
% Returns:
%    Pos: the 1-based position for Elem in List
%
% Notes:
%   This predicate is similar to member/2 with the important difference that we don't bind variables.
%   This is important when either Elem is a variable or List is a list of variables.
%
% E.g.
%   member_identical(D,[1,2,A,D,X]), % succeeds
%   member_identical(D,[1,2,A,D,X], 4)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member_identical(Elem, List):-
  member_identical_pos(Elem, List, _).

member_identical_pos(E1, [E2|_], N, N):-
  E1==E2. % we use == rather than = because we don't want to bind variables
member_identical_pos(E1, [_|T], CurN, N):-
  CurN1 is CurN+1,
  member_identical_pos(E1, T, CurN1, N).

member_identical_pos(Elem, List, N):-
  member_identical_pos(Elem, List, 1, N).

member_identical_chk(E1, [E2|_]):-
  E1==E2, !.
member_identical_chk(E1, [_|T]):-
  member_identical_chk(E1, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% length_up_to(+List, +MaxLength, -Length)
%
% Given:
%   List: a list
%   MaxLength: maximum length to consider in list (>=0)
%              (MaxLength<0 is equivalent to length/2)
%
% Returns:
%   Length: list List length (up to MaxLength)
%
% E.g.
%   length_up_to([a,b,c,d,e], 3, R), R=3.
%   length_up_to([a,b,c,d,e], 9, R), R=5.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_up_to(L, MaxLength, Length):-
  length_up_to(L, MaxLength, 0, Length).

length_up_to([], _, Length, Length):-!.
length_up_to(_, MaxLength, MaxLength, MaxLength):-!.
length_up_to([_|T], MaxLength, CurLength, FLength):-
  CurLength1 is CurLength+1,
  length_up_to(T, MaxLength, CurLength1, FLength).


createList(0, _, []):-!.
createList(N, E, [E|T]):-
  N1 is N-1,
  createList(N1, E, T).


list_to_array(Elements, Array):-
  Array=..[f|Elements].


numbersList(N, N, [N]):-!.
numbersList(CurN, N, [CurN|R]):-
  CurN1 is CurN+1,
  numbersList(CurN1, N, R).



splitAtPos(L, N, N, [], L).
splitAtPos([E|T], CurN, MaxN, [E|R], Remain):-
  (ground(MaxN)-> CurN<MaxN ; true),
  CurN1 is CurN+1,
  splitAtPos(T, CurN1, MaxN, R, Remain).


splitAtPos(L, MaxN, Before, Remain):-
  splitAtPos(L, 0, MaxN, Before, Remain).


firstNElemsOf([], _, _, []):-!.
firstNElemsOf([E|_], N, N, [E]):-!.
firstNElemsOf([E|T], CurN, N, [E|R]):-
  CurN1 is CurN+1,
  firstNElemsOf(T, CurN1, N, R).


firstNElemsOf(_, 0, []):-!.
firstNElemsOf(List, N, Elems):-
  firstNElemsOf(List, 1, N, Elems).


afterFirstNElemsOf(List, N, Result):-
  afterFirstNElemsOf(List, 1, N, Result).


afterFirstNElemsOf([], _, _, []):-!.
afterFirstNElemsOf(List, N, N, List):-!.
afterFirstNElemsOf([_|T], CurN, N, R):-
  CurN1 is CurN+1,
  afterFirstNElemsOf(T, CurN1, N, R).


replaceAt(L, Pos, Elem, R):-
  replaceAt(L, 1, Pos, Elem, R).



%replaceAt(+List, +CurPos, +Pos, +Elem, -NList).
replaceAt([], _, _, _, []):-!.
replaceAt([_|T], Pos, Pos, Elem, [Elem|T]):-!.
replaceAt([H|T], CurPos, Pos, Elem, [H|R]):-
  CurPos1 is CurPos+1,
  replaceAt(T, CurPos1, Pos, Elem, R).



elemsAtPos(_, [], _, []):-!.
elemsAtPos([V|Values], [CurPos|Positions], CurPos, [V|Result]):-
  !,CurPos1 is CurPos+1,
  elemsAtPos(Values, Positions, CurPos1, Result).
elemsAtPos([_|Values], Positions, CurPos, Result):- % Head of Positions > CurPos
  CurPos1 is CurPos+1,
  elemsAtPos(Values, Positions, CurPos1, Result).


split([Elem|T], Elem, [], T).
split([H|T], Elem, [H|Before], After):-
  split(T, Elem, Before, After).


elemsAtPos(Values, Positions, Result):-
  elemsAtPos(Values, Positions, 1, Result).


order_list(InList, OutList):-
  relative_position(InList, TList),
  keysort(TList, OutList).



relative_position(InList, OutList):-
  relative_position(InList, 1, OutList).



relative_position([], _, []):-!.
relative_position([H|T], Pos, [H-Pos|R]):-
  Pos1 is Pos+1,
  relative_position(T, Pos1, R).



find_minimum([K-V|L], MinKey, [BP|R]):-
  find_minimum(L, MinKey, K-V, BP, R). %BP stands for BestPair



%find_minimum(+List, +MinimumKey, +CurBestPair, -FinalBestPair, -MinList)
find_minimum(L, MK, K-V, K-V, L):-
  K=<MK, !.
find_minimum([], _, BP, BP, []):-!.
find_minimum([K1-V1|L], MK, K0-V0, BP, [KH-VH|R]):-
  (K1<K0 ->
    KH=K0, VH=V0,
    NK0=K1, NV0=V1
   ;
    KH=K1, VH=V1,
    NK0=K0, NV0=V0
  ),
  find_minimum(L, MK, NK0-NV0, BP, R).


tuples_to_lists([], List):-
  close_list(List), !. % cut to avoid infinite recursion if initial Tuples is the empty list

tuples_to_lists([Tuple|Tuples], List):-
  Tuple=..[_|Args],
  add_to_list(Args, List, ListTails),
  tuples_to_lists(Tuples, ListTails).

close_list([]).
close_list([[]|T]):-
  close_list(T).



%add_to_list(+Values, +List, -Tails)
add_to_list([], [], []).
add_to_list([Arg|Args], [[Arg|T]|L], [T|R]):-
  add_to_list(Args, L, R).



lists_to_tuples([], _, []):-!. % empty list case
lists_to_tuples([[]|_], _, []):-!. % base case of recursion
lists_to_tuples(List, FunctorName, [Tuple|Tuples]):-
  add_to_list(Args, List, ListTails),
  Tuple=..[FunctorName|Args],
  lists_to_tuples(ListTails, FunctorName, Tuples).



merge_keys(IL, OL):-
  keysort(IL, KSIL), % we must sort the initial list for identical keys to appear consecutively
  merge_sorted_keys(KSIL, OL).

merge_sorted_keys([], []):-!.
merge_sorted_keys([K-V|R], OL):-
  merge_sorted_keys_aux([K-[V]|R], OL).

%:- trust_pred sort(list(T), list(T)).
%:- pred merge_sorted_keys_aux(list(pair(K,list(V))), list(pair(K,list(V)))).

merge_sorted_keys_aux([K-V1], [K-V2]):-
  !, sort(V1, V2).
merge_sorted_keys_aux([K1-V1, K2-V2|R], ROL):-
  (K1==K2 ->
     merge_sorted_keys_aux([K1-[V2|V1]|R], ROL)
    ;
     sort(V1, SV1),
     ROL= [K1-SV1|OL],
     merge_sorted_keys_aux([K2-[V2]|R], OL)
  ).



append_keys(IL, OL):-
  keysort(IL, KSIL), % we must sort the initial list for identical keys to appear consecutively
  append_sorted_keys(KSIL, OL).

append_sorted_keys([], []):-!.
append_sorted_keys([K-V|R], OL):-
  append_sorted_keys_aux([K-[V]|R], OL).

append_sorted_keys_aux([K-V], [K-V]):-!.
append_sorted_keys_aux([K1-V1, K2-V2|R], ROL):-
  (K1==K2 ->
     append_sorted_keys_aux([K1-[V2|V1]|R], ROL)
    ;
     ROL= [K1-V1|OL],
     append_sorted_keys_aux([K2-[V2]|R], OL)
  ).



custom_qsort(L, Compare, SL):-
  custom_qsort_aux(L, Compare, SL-[]).


custom_qsort_aux([], _, T-T):-!.
custom_qsort_aux([A], _, [A|T]-T):-!.
custom_qsort_aux([H|T], Compare, S-R):-
  custom_partition(T, Compare, H, SH, LH),
  custom_qsort_aux(SH, Compare, S-[H|T1]),
  custom_qsort_aux(LH, Compare, T1-R).



custom_partition([], _, _, [], []).
custom_partition([H|T], Compare, E, NSE, NLE):-
  call_with_args(Compare, Result, H, E),
  custom_partition_aux(Result, H, NSE, NLE, SE, LE),
  custom_partition(T, Compare, E, SE, LE).


%custom_partition_aux(+CompareRes, +Elem, -SmallerThanPivot, -LargerThanPivot, -RemainSmallerThanPivot, -RemainLargerThanPivot)
custom_partition_aux(<, Elem, [Elem|SP], LP, SP, LP).
custom_partition_aux(=, _Elem, SP, LP, SP, LP). % ignore element
custom_partition_aux(>, Elem, SP, [Elem|LP], SP, LP).

randomPairs(0, _, []):-!.
randomPairs(N, L, [Pair|Pairs]):-
  randomPair(L, Pair),
  N1 is N - 1,
  randomPairs(N1, L, Pairs).


%randomPair(+List, -Pair)
randomPair(L, (E1, E2)):-
  length(L, N),
  random(1, N, Pos1),
  nth(Pos1, L, E1, RemainL),
  N1 is N-1,
  random(1, N1, Pos2),
  nth(Pos2, RemainL, E2).


%random_elem(+List, -Pos, -Elem)
random_elem(List, Elem):-
  random_elem(List, _, Elem).

random_elem(List, Pos, Elem):-
  random_elem(List, Pos, Elem, _).

random_elem(List, Pos, Elem, RList):-
  length(List, N),
  random(1, N, Pos),
  nth(Pos, List, Elem, RList).


randomMember(Elem, List):-
  randomMember(Elem, _, List).


randomMember(Elem, RandomPos, List) :-
  add_random_keys(List, 1, KeyList), % the idea is to add random numbers as keys
  keysort(KeyList, OrderedKeyList),  % sort according to the keys (effectively, having the list sorted)
  member(_-(RandomPos, Elem), OrderedKeyList). % return the RandomPos and Element

randomPermutation(InList, OutList):-
    add_random_keys(InList, 1, KeyList),
    keysort(KeyList, OrderedKeyList),
    maplist(removeKeyPos, OrderedKeyList, OutList).

removeKeyPos(_Key-(_Position, Elem), Elem).


% add_random_keys(+List, +Pos, -List(RandomFloat-(Pos,Elem))
add_random_keys([], _, []).
add_random_keys([E| Es], N, [K-(N,E)| Ks]) :- % K-(N,E) is Key-(Position, Element)
  K is random, % K is a float between 0 and 1
  N1 is N+1,
  add_random_keys(Es, N1, Ks).


n_randomElems(InList, N, OutList):-
  randomPermutation(InList, SInList),
  firstNElemsOf(SInList, N, OutList).



remove_after([], _, []):-!.
remove_after([H|T], N, [H|R]):-
  H=<N,!,
  remove_after(T, N, R).
remove_after(_, _, []):-!.



remove_positions(List, Positions, NList):-
  remove_positions(List, Positions, 1, NList).

%remove_positions(+List, +Positions, +CurPos, -NList)
remove_positions([], _Positions, _CurPos, []).
remove_positions(L, [], _CurPos, L).

remove_positions([H|T], [Pos|RPos], CurPos, NList):-
  CurPos1 is CurPos+1,
  (Pos==CurPos -> 
      NPos = RPos,
      RList = NList
    ; 
      NPos = [Pos|RPos],
      NList=[H|RList]
   ),
  remove_positions(T, NPos, CurPos1, RList).

test:- write(hello).

:- use_module('../type_checker/type_check'). % enabling the type checker has a 1-2% runtime cost



:- pred createList(integer, T, list(T)).

:- pred numbersList(integer, integer, list(integer)).

:- pred splitAtPos(list(T), integer, integer, list(T), list(T)).

:- pred splitAtPos(list(T), integer, list(T), list(T)).

:- pred firstNElemsOf(list(T), integer, integer, list(T)).


:- pred firstNElemsOf(list(T), integer, list(T)).

:- pred afterFirstNElemsOf(list(T), integer, list(T)).


:- pred afterFirstNElemsOf(list(T), integer, integer, list(T)).

:- pred replaceAt(list(T), integer, T, list(T)).

:- pred replaceAt(list(T), integer, integer, T, list(T)).

:- pred split(list(T), T, list(T), list(T)).


% elemsAtPos(+ValuesList, +PositionsList, +CurPos, -ValuesAtPositionsList)

:- pred elemsAtPos(list(T), list(integer), integer, list(T)).

:- pred elemsAtPos(list(T), list(integer), list(T)).

:- pred relative_position(list(T), list(pair(T,integer))).


:- pred relative_position(list(T), integer, list(pair(T,integer))).

:- pred order_list(list(T), list(pair(T,integer))).


:- pred find_minimum(list(pair(Key,Value)), Key, list(pair(Key,Value))).


:- pred find_minimum(list(pair(Key,Value)), Key, pair(Key,Value), pair(Key,Value), list(pair(Key,Value))).


:- pred add_to_list(list(T), list(list(T)), list(list(T))).


:- meta_predicate custom_qsort(?, :, ?).

:- meta_predicate custom_qsort_aux(?, :, ?).


:- meta_predicate custom_partition(?, :, ?, ?, ?).


%:- pred elemsAtPos(list(T), list(integer), integer, list(T)).

:- pred randomPermutation(list(T), list(T)).
:- trust_pred keysort(list(pair(Key,Value)), list(pair(Key,Value))).
:- trust_pred maplist(pred(A, B), list(A), list(B)).


:- type tuple(A,B) ---> (A,B).
:- pred removeKeyPos(pair(_Key, tuple(_Pos, Elem)), Elem).


:- pred add_random_keys(list(T), integer, list(pair(float,tuple(integer,T)))).

:- pred n_randomElems(list(T), integer, list(T)).

:- type_check_options([check(off), verbose(off)]). 