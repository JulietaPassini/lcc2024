

:- module(proylcc,
	[  
		put/8, solveNonogram/4,check_initial_clues/5
	]).

:-use_module(library(lists)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),
	investList(NewGrid,InvertedGrid),
	numberOfRows(NewGrid,ColLenght),
	getColStateList(ColLenght,ColLenght,InvertedGrid,ColN,_Counter, List), 
	nth0(ColN,ColsClues,ColNClues),
	checkLists(List,ColNClues,ColSat),
	numberOfCols(NewGrid,RowLenght),
	nth0(RowN,NewGrid,RowNList),
	getRowStateList(RowLenght,RowLenght,RowNList,_CounterR,RList),
	investList(RList,RListInverted),
	nth0(RowN,RowsClues,RowNClues),
	checkLists(RListInverted,RowNClues,RowSat). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getRowStateList(_,1, [Body], Counter, List) :-
	(Body == "#" -> Counter = 1; Counter = 0),
	List = [].
	
getRowStateList(FinalRow,RowLength,[Head|Body],Counter,FinalList):-
	RowLength > 0,
	RowLengthAux is RowLength-1,
	getRowStateList(FinalRow, RowLengthAux,Body, FormerCounter,FormerList),
	(Head == "#" -> NewCc = Head ; NewCc = "0"),
	processCases(NewCc,FormerCounter,FormerList,Counter,List),
	processLastInstance(FinalRow, RowLength, List,Counter, FinalList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getColStateList(_,1, [Body], ColN, Counter, List) :-
    nth0(ColN, Body, Cc),
    (Cc == "#" -> Counter = 1; Counter = 0),
    List = [].

getColStateList(FinalCol,ColLength,[Head|Body],ColN,Counter,FinalList):-
		ColLength > 0,
		ColLengthAux is ColLength-1,
		getColStateList(FinalCol, ColLengthAux,Body, ColN, FormerCounter,FormerList),
		nth0(ColN,Head,Cc),
		(Cc == "#" -> NewCc = Cc ; NewCc = "0"),
		processCases(NewCc,FormerCounter,FormerList,Counter,List),
		processLastInstance(FinalCol, ColLength, List,Counter, FinalList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
processCases("0", FormerCounter, FormerList, 0, List) :-
    FormerCounter > 0,
    append(FormerList, [FormerCounter], List).
% 
processCases("0", 0, FormerList, 0, FormerList).
% 
processCases("#",FormerCounter, FormerList, Counter, List) :-
    FormerCounter> 0,
    Counter is FormerCounter + 1,
    List = FormerList.
% 
processCases("#", 0, FormerList, Counter, List) :-
    Counter is 1,
    List = FormerList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------
processLastInstance(FinalCol, ActualCol, List, Counter,FinalList):-
    FinalCol = ActualCol, Counter > 0, 
    append(List, [Counter], FinalList).
%-------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processLastInstance(_, _, List,_, List). % Si no se cumple la condición, FinalList es igual a List

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Comprueba si dos Lists son iguales
checkLists([], [], true).
checkLists([X|Xs], [Y|Ys], Result) :-
    X = Y,
    checkLists(Xs, Ys, Result).
checkLists(_, _, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numberOfCols([Row|_], NumCols) :-
    length(Row, NumCols).
numberOfRows(Grid, NumRows) :-
    length(Grid, NumRows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

investList([], []). 
investList([X|Xs], Y) :- 
    investList(Xs, InvertedXs),
    append(InvertedXs, [X], Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%---------Chequeo inicial de pistas------%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_initial_clues(RowsClues, ColClues, InitialGrid, StateRowsClues, StateColsClues):-
    numberOfRows(InitialGrid, NRows),
    numberOfCols(InitialGrid, NCols),
    getInitialRowsCluesState(NRows, NRows, InitialGrid, InitialRCluesList),
    investList(InitialGrid,InitialInvertedGrid),
    getInitialColsCluesState(NCols, NCols, InitialInvertedGrid, InitialCCluesList),
    checkCluesList(NRows, RowsClues, InitialRCluesList, StateRowsClues),
    checkCluesList(NCols, ColClues, InitialCCluesList, StateColsClues).

getInitialRowsCluesState(Length, 1, InitialGrid, List):-
    nth0(0,InitialGrid,RowN),
    getRowStateList(Length, Length, RowN, _Counter, Result),
    reverse(Result, ReversedResult),
    append([ReversedResult],[],List).

getInitialRowsCluesState(Length, NRow, InitialGrid, InitialRCluesList):-
    NRowAux is NRow-1,
    getInitialRowsCluesState(Length, NRowAux, InitialGrid, FormerList),
    nth0(NRowAux,InitialGrid,RowN),
    getRowStateList(Length, Length, RowN, _Counter, List),
    reverse(List, ReverseList),
    append(FormerList, [ReverseList], InitialRCluesList).

getInitialColsCluesState(Length, 1, InitialInvertedGrid, Result):-
    getColStateList(Length,Length, InitialInvertedGrid, 0, _Counter, List),
    append([List], [], Result).

getInitialColsCluesState(Length, NCols, InitialInvertedGrid, InitialCCluesList):-
    NColsAux is NCols-1,
    getInitialColsCluesState(Length, NColsAux, InitialInvertedGrid, FormerList),
    getColStateList(Length, Length, InitialInvertedGrid, NColsAux, _Counter, List),
    append(FormerList, [List], InitialCCluesList).

checkCluesList(1, Clues, InitialGrid, [Result]):-
    nth0(0, Clues, Clue),
    nth0(0, InitialGrid, ActualState),
    checkLists(Clue, ActualState, Result).

checkCluesList(Instance, Clues, InitialCluesList, CluesState):-
    Instance > 0,
    InstanceAux is Instance - 1,
    checkCluesList(InstanceAux, Clues, InitialCluesList, PreviousState),
    nth0(InstanceAux, Clues, Clue),
    nth0(InstanceAux, InitialCluesList, ActualState),
    checkLists(Clue, ActualState, Result),    
    append(PreviousState, [Result], CluesState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%----------Segunda etapa----------%%%%%%%%%%%%%%%%%%%%%%%%%%%

solveNonogram(Rows,Cols,InitialGrid,Solution):-
    length(Rows, VSize),
    length(Cols, HSize),
    generateCombos(RowCombos, Rows, HSize, InitialGrid),
	transpose(InitialGrid, TInitialGrid),
    generateCombos(ColsCombos, Cols, VSize,TInitialGrid),
    generateCombosCombinatios(RowCombos,RCombosCombinationsList),
	generateCombosCombinatios(ColsCombos,CCombosCombinationsList),
	findMatchingList(RCombosCombinationsList,CCombosCombinationsList,FinalGrid),
	createGrid(FinalGrid,HSize,Solution).


generateCombos(LineCombos, Lines, Size, InitialGrid) :- generateCombosSupp(1, LineCombos, Lines, Size, InitialGrid).

generateCombosSupp(_, [], [], _, _).
generateCombosSupp(LineNumber, [FinalCombos|LineCombosTail], [Line|LinesTail], Size, InitialGrid) :- 
    findall(Combo, combinate(Line, Size, Combo), Combos),
    NextLineNumber is LineNumber + 1,
    ActualIndex is LineNumber - 1,
    nth0(ActualIndex, InitialGrid, RowGrid),
    discardCombos(Combos,RowGrid,FinalCombos),
    generateCombosSupp(NextLineNumber, LineCombosTail, LinesTail, Size, InitialGrid).

combinate(Line, Size, Combo) :- 
    sum(Sum, Line),
    length(Line, Length),
    CortegeLength is Sum + Length - 1,
    combinateSupp(Line, 1, CortegeLength, Size, [], Combo).

combinateSupp([0], _, _, _, _, []).
combinateSupp([Number], Start, _, Size, PreCombo, Combo) :- 
    End is Size - Number + 1,
    inRange(First, Start, End),
    Last is First + Number - 1,
    range(EndCombo, First, Last),
    append(PreCombo, EndCombo, Combo).
combinateSupp([Number|Tail], Start, CortegeLength, Size, PreCombo, Combo) :-
    End is Size - CortegeLength + 1,
    inRange(First, Start, End),
    Last is First + Number - 1,
    range(EndCombo, First, Last),
    NewCortegeLength is CortegeLength - Number - 1,
    NewStart is Last + 2,
    append(PreCombo, EndCombo, LowCombo),
    combinateSupp(Tail, NewStart, NewCortegeLength, Size, LowCombo, Combo).

sum(0, []).
sum(Sum, [Head|Tail]) :- sum(K, Tail), Sum is K + Head.

inRange(Low, Low, High) :- Low =< High.
inRange(I, Low, High) :- Low < High, LessLow is Low + 1, inRange(I, LessLow, High).

range([N], N, N).
range([Low|Tail], Low, High) :- Low < High, LesLow is Low + 1, range(Tail, LesLow, High).


discardCombos(Combos,RowGrid,FinalCombos):-
    elementPositions("X",RowGrid,Positions),
    filterCombos(Positions,Combos,MismatchedCombos),
	FinalCombos=MismatchedCombos.

findPositions(_, [], [], _).
findPositions(Element, [Element|Rest], [Position|RemaingingPositions], Position) :-
    ActualPosition is Position + 1,
    findPositions(Element, Rest, RemaingingPositions, ActualPosition).
findPositions(Element, [Different|Rest], Positions, Position) :-
    Different \= Element,
    ActualPosition is Position + 1,
    findPositions(Element, Rest, Positions, ActualPosition).
 
 elementPositions(Element, List, Positions) :-
    findPositions(Element, List, Positions, 1).

filterCombos([], Combos, Combos).
filterCombos([X|Xs], Combos, Result) :-
    deleteMatching(X, Combos, FilteredCombos),
    filterCombos(Xs, FilteredCombos, Result).
    
deleteMatching(_, [], []).
deleteMatching(X, [List|Lists], Result) :-
    (   member(X, List)
    ->  deleteMatching(X, Lists, Result)
    ;   deleteMatching(X, Lists, Rest),
    Result = [List|Rest]).


transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [FirstCol|Transposed]) :-
    transpose_1st_col(Matrix, FirstCol, RestMatrix),
    transpose(RestMatrix, Transposed).

transpose_1st_col([], [], []).
transpose_1st_col([[X|Xs]|RestMatrix], [X|Col], [Xs|RestMatrix1]) :-
    transpose_1st_col(RestMatrix, Col, RestMatrix1).


generateCombosCombinatios(List, Combinations) :-
    findall(Combination, combine(List, Combination), Combinations).

combine([], []).
combine([Sublist|Rest], [Element|Combination]) :-
    member(Element, Sublist),
    combine(Rest, Combination).


findMatchingList(A, B, Matched) :-
    findFirstMatchingList(A, B, Matched).

findFirstMatchingList([], _, []).
findFirstMatchingList([List|RestA], ListB, Result) :-
    (   matchSingleListWithAny(ListB, List, true)
    ->  Result = List
    ;   findFirstMatchingList(RestA, ListB, Result)
    ).

matchSingleListWithAny([], _, false).
matchSingleListWithAny([ListB|RestB], List, Result) :-
    (   matchLists(List, ListB, 0)
    ->  Result = true
    ;   matchSingleListWithAny(RestB, List, Result)
    ).

matchLists([], _, _).
matchLists([SubList|RestA], ListB, Index) :-
    Index1 is Index + 1,
    findall(Index1, between(1, Index1, Index1), Positions),
    checkPositions(Index1, SubList ,Positions, ListB),
    matchLists(RestA, ListB, Index1).

checkPositions(_, [], _).
checkPositions(Element, [PositionA|PositionsA], Positions, ListOfLists) :-
    nth1(PositionA, ListOfLists, SubList),
    member(Element, SubList),
    checkPositions(Element, PositionsA,Positions, ListOfLists),!.
checkPositions(Element,[PositionA],_,ListOfLists):-
    nth1(PositionA, ListOfLists, SubList),
    member(Element, SubList).
    
createGrid([], _GridLength, []).
createGrid([Row|Rows], GridLength, [PopulatedRow|PopulatedRows]) :-
    createRow(Row, GridLength, PopulatedRow),
    createGrid(Rows, GridLength, PopulatedRows).


createRow(NumberPositions, RowLength, ResultRow) :-
    populateRow(NumberPositions, RowLength, 1, ResultRow).

populateRow([], RowLength, CurrentIndex, ResultRow) :-
    fillWithEmpty(CurrentIndex, RowLength, ResultRow).


populateRow([Position|Positions], RowLength, CurrentIndex, [Cell|ResultRow]) :-
    CurrentIndex =:= Position, 
    Cell = "#",
    NextIndex is CurrentIndex + 1,
    populateRow(Positions, RowLength, NextIndex, ResultRow).

populateRow([Position|Positions], RowLength, CurrentIndex, [Cell|ResultRow]) :-
    CurrentIndex =\= Position,
    Cell = "X",
    NextIndex is CurrentIndex + 1,
    populateRow([Position|Positions], RowLength, NextIndex, ResultRow).


fillWithEmpty(CurrentIndex, RowLength, ResultRow) :-
    CurrentIndex =< RowLength,
    append(["X"], RestRow, ResultRow),
    NextIndex is CurrentIndex + 1,
    fillWithEmpty(NextIndex, RowLength, RestRow).

fillWithEmpty(CurrentIndex, RowLength, []) :-
    CurrentIndex > RowLength.



