import curses
import time

def getTestInput():
    smallInput = [
        "..@@.@@@@.",
        "@@@.@.@.@@",
        "@@@@@.@.@@",
        "@.@@@@..@.",
        "@@.@@@@.@@",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "@.@@@.@@@@",
        ".@@@@@@@@.",
        "@.@.@@@.@.",
    ]
    return [list(l) for l in smallInput]

def nbNeighbors(lines, x, y):
    neighbors = 0
    for i in range(x-1, x+2):
        for j in range(y-1, y+2):
            if i == x and j == y:
                continue
            if 0 <= i < len(lines) and 0 <= j < len(lines[0]) and lines[i][j] == '@':
                neighbors += 1
    return neighbors

def drawGrid(stdscr, grid, highlights):
    rows, cols = stdscr.getmaxyx()
    gridStartX = int((cols - 2 * len(grid)) / 2)
    gridStartY = int((rows - len(grid[0])) / 2) - 2
    for y, row in enumerate(grid):
        for x, ch in enumerate(row):
            try:
                stdscr.addch(y + gridStartY, 2 * x + gridStartX, ch)
                stdscr.addch(y + gridStartY, 2 * x + gridStartX + 1, ' ')
            except curses.error:
                pass  # Prevent crash if drawing oob
    for [x, y] in highlights:
        drawHighlight(stdscr, grid, x, y, curses.color_pair(2))

def drawHighlight(stdscr, grid, y, x, color):
    rows, cols = stdscr.getmaxyx()
    gridStartX = int((cols - 2 * len(grid)) / 2)
    gridStartY = int((rows - len(grid[0])) / 2) - 2
    try:
        stdscr.addch(y + gridStartY, 2 * x + gridStartX, grid[y][x], color)
    except curses.error:
        pass  # Prevent crash if drawing oob

def drawResult(stdscr, result, grid, color):
    rows, cols = stdscr.getmaxyx()
    resultStartX = int((cols - 2 * len(grid)) / 2)
    resultStartY = int((rows + len(grid[0])) / 2) - 1
    try:
        stdscr.addstr(resultStartY, resultStartX, "Total removed:        ")
        stdscr.addstr(resultStartY, resultStartX + 15, str(result), color)
    except curses.error:
        pass  # Prevent crash if drawing oob

def drawModified(stdscr, modified, grid):
    rows, cols = stdscr.getmaxyx()
    resultStartX = int((cols - 2 * len(grid)) / 2)
    resultStartY = int((rows + len(grid[0])) / 2) + 1
    try:
        stdscr.addstr(resultStartY, resultStartX, "Modified:       ")
        stdscr.addstr(resultStartY, resultStartX + 10, str(modified), curses.color_pair(4) if modified else curses.color_pair(3))
    except curses.error:
        pass  # Prevent crash if drawing oob

def drawExitMessage(stdscr, grid):
    rows, cols = stdscr.getmaxyx()
    resultStartX = int((cols - 2 * len(grid)) / 2)
    resultStartY = int((rows + len(grid[0])) / 2) + 1
    try:
        stdscr.addstr(resultStartY, resultStartX, "Press q to exit")
    except curses.error:
        pass  # Prevent crash if drawing oob

def main(stdscr):
    # Init
    curses.curs_set(0)
    curses.use_default_colors()
    stdscr.nodelay(True)
    curses.init_pair(1, -1, curses.COLOR_RED)
    curses.init_pair(2, -1, curses.COLOR_GREEN)
    curses.init_pair(3, curses.COLOR_RED, -1)
    curses.init_pair(4, curses.COLOR_GREEN, -1)
    grid = getTestInput()
    result = 0
    highlights = []
    
    # Draw
    stdscr.clear()
    drawGrid(stdscr, grid, highlights)
    drawResult(stdscr, result, grid, curses.color_pair(0))
    stdscr.refresh()

    # Algorithm
    finished = False
    while not finished:
        time.sleep(1)

        finished = True
        highlights = []
        for i in range(len(grid)):
            for j in range(len(grid[0])):
                time.sleep(0.05)

                modified = False
                if grid[i][j] == '@' and nbNeighbors(grid, i, j) < 4:
                    result += 1
                    grid[i][j] = '.'
                    highlights.append([i, j])
                    finished = False
                    modified = True
                
                # Draw
                key = stdscr.getch()
                drawGrid(stdscr, grid, highlights)
                drawHighlight(stdscr, grid, i, j, curses.color_pair(2) if modified else curses.color_pair(1))
                drawModified(stdscr, not finished, grid)
                drawResult(stdscr, result, grid, curses.color_pair(0))
                stdscr.refresh()

    # Wait for exit
    time.sleep(1)
    highlights = []
    stdscr.clear()
    while True:
        key = stdscr.getch()
        if key == ord('q'):
            break
        drawGrid(stdscr, grid, highlights)
        drawResult(stdscr, result, grid, curses.color_pair(4))
        drawExitMessage(stdscr, grid)
        stdscr.refresh()
        time.sleep(0.1)


if __name__ == "__main__":
    curses.wrapper(main)
