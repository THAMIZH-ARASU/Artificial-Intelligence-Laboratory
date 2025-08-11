import heapq

def heuristic(a, b):
    # Manhattan Distance
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def get_neighbors(pos, rows, cols):
    x, y = pos
    directions = [(0,1), (1,0), (0,-1), (-1,0)]
    return [(x+dx, y+dy) for dx, dy in directions if 0 <= x+dx < rows and 0 <= y+dy < cols]

def print_set(name, s):
    print(f"{name}: {[str(x) for x in s]}")

def astar(grid, start, goal):
    rows, cols = len(grid), len(grid[0])
    open_set = [(0, start)]
    heapq.heapify(open_set)
    came_from = {}
    g_score = {start: 0}
    closed_set = set()

    step = 1
    while open_set:
        print(f"\n--- Step {step} ---")
        print_set("Open Set", [item[1] for item in open_set])
        print_set("Closed Set", closed_set)

        _, current = heapq.heappop(open_set)
        closed_set.add(current)

        if current == goal:
            path = []
            while current in came_from:
                path.append(current)
                current = came_from[current]
            path.append(start)
            return path[::-1]

        for neighbor in get_neighbors(current, rows, cols):
            x, y = neighbor
            if grid[x][y] == '#':
                continue
            if neighbor in closed_set:
                continue

            tentative_g = g_score[current] + 1
            if neighbor not in g_score or tentative_g < g_score[neighbor]:
                g_score[neighbor] = tentative_g
                f_score = tentative_g + heuristic(neighbor, goal)
                heapq.heappush(open_set, (f_score, neighbor))
                came_from[neighbor] = current

        step += 1

    return None

def main():
    print("Enter number of rows and columns:")
    rows, cols = map(int, input().split())

    print("\nEnter the maze row by row using:")
    print("  S - Start")
    print("  G - Goal")
    print("  . - Free path")
    print("  # - Obstacle\n")

    grid = []
    start = goal = None
    for i in range(rows):
        row = list(input(f"Row {i+1}: "))
        if len(row) != cols:
            print("Invalid input length! Each row must have", cols, "characters.")
            return
        for j in range(cols):
            if row[j] == 'S':
                start = (i, j)
            elif row[j] == 'G':
                goal = (i, j)
        grid.append(row)

    if not start or not goal:
        print("Maze must contain a Start (S) and Goal (G).")
        return

    print("\nFinding path using A* algorithm...\n")
    path = astar(grid, start, goal)

    if path:
        print("\n✅ Path found:")
        for step in path:
            print(step)
        print("\nMaze with path:")
        for i, j in path[1:-1]:  # exclude start and goal from marking
            grid[i][j] = '*'
        for row in grid:
            print(''.join(row))
    else:
        print("❌ No path found.")

if __name__ == "__main__":
    main()

#  S..#..
# Row 2: #.#.#.
# Row 3: #.#...
# Row 4: ...#.G
# 5 7

# Row 1: S..#...
# Row 2: #.#.#.#
# Row 3: #...#.#
# Row 4: ###.#.#
# Row 5: ......G


# .S..
# .##.
# ..G.