# What a_star.pl should do : 
    - Given a board that consists of N x M cells.
    - Each cell contains a color (Red, Yellow or Blue).
    - Given a start and goal cells of the same color.
    - Find if there is a path between the start and the end cells.
    - All cells on the path must be from the same color.
    - Diagonal moves are not allowed. You can move left, right, up and down.
    - Path must not contain repeated cells.
    
# To use A* Algorithm use this query : 
    a_star_search([[start, null, 0, x, 0]], [], goal).
    replace start with the node you want to start with, and goal woth the goal you want 
    Example: a_star_search([[node(0, 0, red), null, 0, x, 0]], [], node(1, 3, red)).
