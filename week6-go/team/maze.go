package main

import (
	"bufio"
	"fmt"
	"os"
	"sync"
    "errors"
)

const noWall byte = (0) // The first flag
const southWall byte = (1 << 0) // The first flag
const eastWall byte = (1 << 1)  // The second flag

type Maze struct {
	walls [][]byte
	cells [][]int
	r,c int
	}
type Position struct {
	Row, Col int
}

/* EXAMPLE: for a Position pos, there is NO wall north of pos if and only if
 *  maze[pos.Row - 1][pos.Col] & southWall == 0
 *
 * Use the above construct, including the '== 0' part, when checking for the
 * absence of walls. For an explanation of why this works, see the provided
 * 'Questions and Answers' document.
 */

func readMaze(f *os.File) (maze Maze, err error) {

	s := bufio.NewScanner(f)

	var rows int
	var cols int
	var walls [][]byte
	for s.Scan() {

		cols ++

        b := s.Text()
        for _, bt := range b {

			rows ++
            // the possible input runes are {0, 1, 2, 3} (48-51 in Unicode)
            if bt < 48 || bt > 51 {
                return Maze{}, errors.New("incorrect input given, exiting.")
            }
        }

		byt := []byte(b)
		walls = append(walls, byt)
	}
	cells := make([][]int, rows)
	for i := range cells {
		cells[i] = make([]int, cols)
	}
	maze = Maze{walls, cells, 0, 0}
	return maze, nil
}

func solve(maze Maze, goal_r int, goal_c int) (route Maze, err error) {

	const max_path_length int = 200
	// initialize 2D onceMaze of equivalent size as the input maze
	var onceMaze = make([][]sync.Once, len(maze.walls) + 1)
	for i := range onceMaze {
    	onceMaze[i] = make([]sync.Once, len(maze.walls[0]) + 1)
	}

	// routes
	routes := make(chan Maze )

	maze.cells[0][0] = 1

	routes <- maze

	for {



		// Get the route to further explore
		route, more := <- routes

		fmt.Println(route)

		// get the last explored coordinate
		r := route.r
		c := route.c

		// stop if we found the goal
		if r == goal_r && c == goal_c {

			close(routes)
			return route, nil
		}

		if more {

			richtingen := make(chan Maze)
			go step(onceMaze, maze, richtingen)

			for richting := range richtingen {



				routes <- richting
			}

		} else {
			close(routes)
	        return Maze{}, errors.New("incorrect input given, exiting.")
		}
	}

	close(routes)
	return Maze{},  errors.New("incorrect input given, exiting.")
}



func step(once [][]sync.Once, maze Maze, richtingen chan Maze) {

	row := maze.r
	col := maze.c

	boundsr := len(maze.walls) - 1
	boundsc := len(maze.walls[0]) - 1


	once[row][col].Do(func() {
		// Add to possible direction to the channel

		var pos byte = byte(maze.walls[row][col])


		switch {

		case pos == 48:
			if col < boundsc && maze.cells[row][col+1] == 0 {
				new_maze := maze
				new_maze.cells[row][col+1] = 1
				richtingen <- new_maze
			}

			if row < boundsr && maze.cells[row+1][col] == 0 {
				new_maze := maze
				new_maze.cells[row+1][col] = 1
				richtingen <- new_maze
			}

		case pos == 49:

			if col < boundsr && maze.cells[row][col+1] == 0 {
				new_maze := maze
				new_maze.cells[row][col+1] = 1
				richtingen <- new_maze
			}

		case pos == 50:

			if col < boundsr && maze.cells[row+1][col] == 0 {
				new_maze := maze
				new_maze.cells[row+1][col] = 1
				richtingen <- new_maze
			}

		default:

		}

		if row != 0 {

			if ((maze.walls[row - 1][col] == 48) || (maze.walls[row - 1][col] == 50)) && maze.cells[row-1][col] == 0 {
				new_maze := maze
				new_maze.cells[row-1][col] = 1
				richtingen <- new_maze
			}
		}

		if col != 0 {
			if ((maze.walls[row][col - 1]) == 48 || (maze.walls[row][col - 1] == 49)) && maze.cells[row][col-1] == 0 {
				new_maze := maze
				new_maze.cells[row][col-1] = 1
				richtingen <- new_maze
			}
		}
	})

	close(richtingen)
}

func main() {

    if len(os.Args) != 2 {
        fmt.Println("incorrect amount of arguments.")
        os.Exit(1)
    }

	f, err := os.Open(os.Args[1])

	if err != nil {
		fmt.Println(err)
		os.Exit(2)
	}

	maze, read_err := readMaze(f)

    if read_err != nil {
		fmt.Println(read_err)
		os.Exit(3)
	}

	goal_r := len(maze.walls) - 1
	goal_c := len(maze.walls[0]) - 1
	route, maze_error := solve(maze, goal_r, goal_c)

	if maze_error != nil {
		fmt.Println(maze_error)
		os.Exit(4)
	}

	// for _, pos := range route {
	// 	maze[pos.Row][pos.Col] |= byte(1 << 2) // The third flag
	// }

	for _, row := range route.cells {
		fmt.Println(row)
	}

	for _, line := range maze.walls {
		fmt.Println(string(line))
	}
}
