% ------------------------------------------------------ board input ------------------------------------------------

% Predicate to take input as list of lists of colors
input_board(Board) :-
    write('Enter the board, Ex -> [[r, b], [y, y]]: '),
    read(Board).

% Predicate to get board dimensions
get_board_dimensions(Board, NumRows, NumColumns) :-
    length(Board, NumRows),
    Board = [Row|_],
    length(Row, NumColumns).

% ---------------------------------------------------- logic of puzzle ---------------------------------------------

% Define valid moves without diagonal moves
% Move right
move(X, Y, X, Y1, _, NumColumns) :- 
    Y1 is Y + 1, 
    Y1 < NumColumns. 
% Move left
move(X, Y, X, Y1, _, _) :- 
    Y1 is Y - 1, 
    Y1 >= 0.  
% Move down       
move(X, Y, X1, Y, NumRows, _) :- 
    X1 is X + 1, 
    X1 < NumRows.
% Move up
move(X, Y, X1, Y, _, _) :- 
    X1 is X - 1,
    X1 >= 0.

% Check if a position is within the bounds of the board
inbounds(X, Y, Board, Color) :-
    nth0(X, Board, Temp), % Get the row at index X
    nth0(Y, Temp, Color1), % Get the cell at index Y in the row
    Color1 == Color.

% Check if the path forms a cycle with a given color
check_cycle_in_path(Path, Color, Board) :-
    length(Path, Length),
    Length >= 4,
    Path = [(X1, Y1)|_],
    last(Path, (X2, Y2)),
    get_board_dimensions(Board, NumRows, NumColumns),
    move(X1, Y1, X2, Y2, NumRows, NumColumns),
    inbounds(X2, Y2, Board, Color).

% ---------------------------------------------------- logic of BFS search ---------------------------------------------

% Find a cycle of a given color
find_cycle_of_color(Color,Board, Cycle) :-
    findall(
        [X, Y, Color, [(X, Y)]], % open list
        (
            inbounds(X, Y, Board, Color)
        ),
        StartStates
    ),
    bfs_search(StartStates, Board, Color, Cycle).

% Breadth-first search base case, it stops when the path forms a cycle
bfs_search([[X, Y, Color, Path] | _], Board, Color, Cycle) :-
    check_cycle_in_path(Path, Color, Board),
    reverse(Path, NewPath),
    Cycle = [Color, NewPath].

bfs_search([[X, Y, Color, Path] | RestOpenList], Board, Color, Cycle) :-
    get_board_dimensions(Board, NumRows, NumColumns),
    findall(
        [X1, Y1, Color, [(X1, Y1) | Path]], % closed list
        (
            move(X, Y, X1, Y1, NumRows, NumColumns),
            inbounds(X1, Y1, Board, Color),
            \+ member((X1, Y1), Path)
        ),
        States
    ),
    append(RestOpenList, States, NewRestOpenList),
    bfs_search(NewRestOpenList, Board, Color, Cycle).

% ---------------------------------------------------- main ---------------------------------------------

% predicate to find any cycle with any color
search(Cycle) :-
    input_board(Board),
    (
        find_cycle_of_color(b, Board, [Color, Path]);
        find_cycle_of_color(r, Board, [Color, Path]);
        find_cycle_of_color(y, Board, [Color, Path])
    ),
    !, % Cut to prevent backtracking after the first solution is found
    nl, write('Found cycle: '), write('color: ( '), write(Color),write(' ) path: '), write(Path).

% no cycle found
search(_) :-
    nl, write('No cycles').

main :-
    nl, search(Cycle).