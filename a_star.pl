% --------------------------------------------------------- Facts ------------------------------------------------

% (width, hieght)
board_size(4, 4).

% grid representation 
node(0,0,red).
node(0,1,red).
node(0,2,yellow).
node(0,3,yellow).
node(1,0,red).  
node(1,1,blue).
node(1,2,red).
node(1,3,red).
node(2,0,red).
node(2,1,red).
node(2,2,red).
node(2,3,yellow).
node(3,0,blue).
node(3,1,red).
node(3,2,blue).
node(3,3,yellow).


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
    board_size(N, M), 
    X >= 0, X < N, 
    Y >= 0, Y < M.

% calculate heuristic predicate using manhattan distance 
heuristic(node(X, Y, C), node(X_Goal, Y_Goal, C_Goal), H) :-
    H is abs(X - X_Goal) + abs(Y - Y_Goal).

% ---------------------------------------------------- logic of A* search ---------------------------------------------

% search base case, it stops when the current state is the same as goal state
a_star_search(Open, Closed, Goal):-
    getBestState(Open, [CurrentState,Parent,G,H,F], _), 
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