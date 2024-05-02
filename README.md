# What a_star.pl should do : 
    - Given a board that consists of N x M cells.
    - Each cell contains a color (Red, Yellow or Blue).
    - Given a start and goal cells of the same color.
    - Find if there is a path between the start and the end cells.
    - All cells on the path must be from the same color.
    - Diagonal moves are not allowed. You can move left, right, up and down.
    - Path must not contain repeated cells.
    
# To use A* Algorithm use this query : 
    - Use (input.) as your query to enter the puzzle you want to solve.
    - Use (search.) as your query to input your start, goal, and solve the puzzle.

# What bfs.pl should do : 
    - Given a board that consists of N x M cells.
    - Each cell contains a color (Red, Yellow or Blue).
    - Your task is to find color cycles for any of the three colors.
    - Print at least one of these cycles including its color (if any) or no cycles exist.
    - Cycle defined by the following cells c1, c2, ..., ck must have the following:
        - These cells are different (no duplicates).
        - At least 4 cells (or more).
        - All cells have the same color.
        - The cells are adjacent to each other.

# To use BFS Algorithm use this query : 
    - (main.)