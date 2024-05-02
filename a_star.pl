% --------------------------------------------------------- Facts ------------------------------------------------

% board_size(NumColumns, NumRows)
:- dynamic board_size/2.

% node(x, y, color)
:- dynamic node/3.

color(r).
color(b).
color(y).

% ------------------------------------------------------ board input ------------------------------------------------

% Predicate to take input for board dimensions
input_board_dimensions :-
    write('Enter the board width (number of columns): '), 
    read(NumColumns),
    write('Enter the board height (number of rows): '),
    read(NumRows),
    assert(board_size(NumColumns, NumRows)).

% Predicate to take input for each node's color
input_board(X, _) :-
    board_size(_, NumRows),
    X = NumRows, !,
    nl, write('Board input complete!'), nl.

input_board(X, Y) :-
    board_size(NumColumns, NumRows),
    Y1 is Y + 1,
    X1 is X + 1,
    (Y1 =< NumColumns -> (input_node(X, Y), input_board(X, Y1)) ; input_board(X1, 0)).

input_node(X, Y) :-
    write('Enter color for node at position ('),
    write(X), write(','), write(Y), write(')'),
    read(Color),
    (color(Color) -> assert(node(X, Y, Color)) ; (nl, write('Invalid color!'), nl, fail)).

inputCompleted(X, Y) :-
    board_size(NumColumns, NumRows),
    X = NumRows, Y = NumColumns.

% ------------------------------------------------------ search input ----------------------------------------------

input_search_data(node(X1, Y1, C1), node(X2, Y2, C2)) :-
    write('Enter the start node, Ex -> node(x, y, color): '),
    read(node(X1, Y1, C1)),
    (node(X1, Y1, C1) -> true ; (nl, write('Invalid node!'), nl, fail)),
    write('Enter the goal node, Ex -> node(x, y, color): '),
    read(node(X2, Y2, C2)),
    (node(X2, Y2, C2) -> true ; (nl, write('Invalid node!'), nl, fail)).

% ---------------------------------------------------- logic of puzzle ---------------------------------------------

% Define valid moves without diagonal moves
% move down
move(node(X1, Y,C), node(X2, Y,C), 1):- 
    X2 is X1 + 1, 
    inbounds(X2, Y),
    node(X1, Y, C), 
    node(X2, Y, C).
% move up
move(node(X1, Y,C), node(X2, Y,C), 1):- 
    X2 is X1 - 1, 
    inbounds(X2, Y),
    node(X1, Y, C), 
    node(X2, Y, C).
% move right
move(node(X, Y1,C), node(X, Y2,C), 1):- 
    Y2 is Y1 + 1, 
    inbounds(X, Y2),
    node(X, Y1, C), 
    node(X, Y2, C).
% move left
move(node(X, Y1,C), node(X, Y2,C), 1):- 
    Y2 is Y1 - 1, 
    inbounds(X, Y2),
    node(X, Y1, C), 
    node(X, Y2, C).

% Check if a position is within the bounds of the board
inbounds(X, Y) :-
    board_size(NumColumns, NumRows), 
    X >= 0, X < NumRows, 
    Y >= 0, Y < NumColumns.

% calculate heuristic predicate using manhattan distance 
heuristic(node(X, Y, C), node(X_Goal, Y_Goal, C_Goal), H) :-
    H is abs(X - X_Goal) + abs(Y - Y_Goal).

% ---------------------------------------------------- logic of A* search ---------------------------------------------

% search base case, it stops when the current state is the same as goal state
a_star_search(Open, Closed, Goal):-
    getBestStateFromFrontier(Open, [CurrentState,Parent,G,H,F], _), 
    CurrentState = Goal, 
    write("search is complete!"), nl,
    printSolution([CurrentState,Parent,G,H,F], Closed), !.

% recursive call for search 
a_star_search(Open, Closed, Goal):-
    getBestStateFromFrontier(Open, CurrentNode, TmpOpen),
    getAllValidChildren(CurrentNode, TmpOpen, Closed, Goal, Children),
    addChildren(Children, TmpOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed),
    a_star_search(NewOpen, NewClosed, Goal).

% when there is no path
a_star_search(_, _, _):-
    write("no path!"), nl.

% get the best state and delete in from the open list (frontier)
getBestStateFromFrontier(Open, BestChild, Rest):-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).    

% get min cost in the open list (frontier)
findMin([X], X):- !.
findMin([Head|T], Min):-
    findMin(T, TmpMin),
    Head = [_, _, _, HeadH, HeadF],
    TmpMin = [_, _, _, TmpH, TmpF],
    (TmpF < HeadF -> Min = TmpMin ; Min = Head).

% fet all the children of the currrent node 
getAllValidChildren(Node, Open, Closed, Goal, Children):-
    findall(
        Next, 
        getNextState(Node, Open, Closed, Goal, Next), 
        Children
    ).

% get the next state data
getNextState([State, _, G, _, _], Open, Closed, Goal, [Next, State, NewG, NewH, NewF]):-
    move(State, Next, MoveCost),
    heuristic(Next, Goal, NewH),
    NewG is G + MoveCost,
    NewF is NewG + NewH,
    (not(member([Next, _, _, _, _], Open))),
    (not(member([Next, _, _, _, _], Closed))).
    
% addChildren to the open list (frontier)
addChildren(Children, Open, NewOpen):-
    append(Open, Children, NewOpen).

% ---------------------------------------------------- print solution ---------------------------------------------

% base case
printSolution([State, null, G, H, F], _):-
    write([State, G, H, F]), nl.
    
printSolution([State, Parent, G, H, F], Closed):-
    member([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    printSolution([Parent, GrandParent, PrevG, Ph, Pf], Closed),
    write([State, G, H, F]), nl.

% ------------------------------------------------------ main ---------------------------------------------

input :- 
    retractall(node(_, _, _)),
    retractall(board_size(_, _)),
    nl,
    input_board_dimensions,
    nl,
    input_board(0, 0).

search :- 
    nl,
    input_search_data(Start, Goal),
    nl,
    a_star_search([[Start,null,0,x,0]], [], Goal).