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

var initialize sync.Once

var boundsr int
var boundsc int

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


	var walls [][]byte
	for s.Scan() {


        b := s.Text()
        for _, bt := range b {


            // the possible input runes are {0, 1, 2, 3} (48-51 in Unicode)
            if bt < 48 || bt > 51 {
                return Maze{}, errors.New("incorrect input given, exiting.")
            }
        }

		byt := []byte(b)
		walls = append(walls, byt)
	}
	cells := make([][]int, len(walls))
	for i := range cells {
		cells[i] = make([]int, len(walls[0]))
	}
	maze = Maze{walls, cells, 0, 0}
	fmt.Println("aantal rijen ", len(cells))
	fmt.Println("aantal kolommen ", len(cells[0]))
	return maze, nil
}



func solve(maze Maze, goal_r int, goal_c int) (route Maze, err error) {


	// initialize 2D onceMaze of equivalent size as the input maze


	// routes
	routes := make(chan Maze, 100)

	var mazeHere Maze
	mazeHere = maze
	maze.cells[0][0] = 1


	routes <- mazeHere

	for {


		// Get the route to further explore
		route, more := <- routes
		// fmt.Println("\n",route.cells,"\n")
		// fmt.Println("huidige route:")
		// fmt.Println(route)
		// fmt.Println("---------------------")

		// for i, row := range route.cells {
		//     	fmt.Println(i, row)
		//     }
		// fmt.Println("\n")
		// for i, wall := range route.walls {
		//     	fmt.Println(i, wall)
		//     }
		// fmt.Println("\n")

		// get the last explored coordinate
		r := route.r
		c := route.c

		// stop if we found the goal
		if r == goal_r && c == goal_c {

			fmt.Println("ROUTE GEVONDEN!!!!!\\n")
			close(routes)
			return route, nil
		}

		if more {

			// richtingen := make(chan Maze, 3)

			go step(route, routes)

			// for richting := range richtingen {
			// 	fmt.Println("nou zeg")
			//
			// 	routes <- richting
			// }

		} else {
			close(routes)
	        return Maze{}, errors.New("incorrect input given, exiting.")
		}
	}

	close(routes)
	return Maze{},  errors.New("incorrect input given, exiting.")
}



func copyMaze(maze Maze) (copiedMaze Maze) {

	copiedMaze = maze
	copiedMaze.cells = make([][]int, len(maze.cells))
	for i, _ := range copiedMaze.cells {
		copiedMaze.cells[i] = make([]int, len(maze.cells[0]))
		copy(copiedMaze.cells[i], maze.cells[i])
	}

	return copiedMaze
}


func step(maze Maze, richtingen chan Maze) {



	initialize.Do(func() {
		boundsr = len(maze.walls) - 1
		boundsc = len(maze.walls[0]) -1
	})


	row := maze.r
	col := maze.c



	// Add to possible direction to the channel

	var pos byte = byte(maze.walls[row][col])

	// if (row > boundsr - 2) || (col > boundsc -2) {
	// 	fmt.Println("position: ", row, ",", col, "bounds: ", boundsr, ",", boundsc)
	// }


	switch {

	case pos == 48:
		if col < boundsc && maze.cells[row][col+1] == 0 {
			// fmt.Println("a")
			new_maze := copyMaze(maze)

			new_maze.c = col + 1
			new_maze.cells[row][col+1] = 1


			// fmt.Println("naar rechts:\n",new_maze.cells, "\n")
			richtingen <- new_maze
		}

		if row < boundsr {
			if maze.cells[row+1][col] == 0 {
				// fmt.Println("b")
				new_maze := copyMaze(maze)

				new_maze.r = row + 1
				new_maze.cells[row+1][col] = 1
				// fmt.Println("naar beneden:\n",new_maze.cells, "\n")
				richtingen <- new_maze
			}
		}

	case pos == 49:

		if col < boundsc {
			 if maze.cells[row][col+1] == 0 {
				// fmt.Println("c")

				new_maze := copyMaze(maze)

				new_maze.c = col + 1
				new_maze.cells[row][col+1] = 1

				// fmt.Println("naar rechts:\n",new_maze.cells, "\n")
				richtingen <- new_maze

			}
		}

	case pos == 50:

		if row < boundsr {
			if maze.cells[row+1][col] == 0 {
				// fmt.Println("d")
				new_maze := copyMaze(maze)

				new_maze.r = row + 1
				new_maze.cells[row+1][col] = 1
				// fmt.Println("naar beneden:\n",new_maze.cells, "\n")
				richtingen <- new_maze
			}
		}

	}

	if row != 0 {

		if ((maze.walls[row - 1][col] == 48) || (maze.walls[row - 1][col] == 50)) && maze.cells[row-1][col] == 0 {
			// fmt.Println("e")
			new_maze := copyMaze(maze)

			new_maze.r = row -1
			new_maze.cells[row-1][col] = 1

			// fmt.Println("naar boven:\n",new_maze.cells, "\n")
			richtingen <- new_maze
		}
	}

	if col != 0 {
		if ((maze.walls[row][col - 1]) == 48 || (maze.walls[row][col - 1] == 49)) && maze.cells[row][col-1] == 0 {
			// fmt.Println("f")
			new_maze := copyMaze(maze)

			new_maze.c = col - 1
			new_maze.cells[row][col-1] = 1
			// fmt.Println("naar links:\n",new_maze.cells, "\n")
			richtingen <- new_maze
		}
	}


	// close(richtingen)
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

	fmt.Println("route printen")
	for _, row := range route.cells {
		fmt.Println(row)
	}

	for _, line := range maze.walls {
		fmt.Println(string(line))
	}
}
