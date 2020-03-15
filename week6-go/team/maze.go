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

type Maze [][]byte

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

	for s.Scan() {

        b := s.Text()
        for _, bt := range b {
            // the possible input runes are {0, 1, 2, 3} (48-51 in Unicode)
            if bt < 48 || bt > 51 {
                return nil, errors.New("incorrect input given, exiting.")
            }
        }

		byt := []byte(b)
		maze = append(maze, byt)
	}
	return maze, nil
}

func solve(maze Maze, goal Position) (route []Position, err error) {

	// initialize 2D onceMaze of equivalent size as the input maze
	var onceMaze = make([][]sync.Once, len(maze) + 1)
	for i := range onceMaze {
    	onceMaze[i] = make([]sync.Once, len(maze[0]) + 1)
	}

	// routes
	routes := make(chan []Position, 200)

	startexplore := make([]Position, 0)
	startexplore = append(startexplore, Position{0, 0})

	routes <- startexplore

	for {



		// Get the route to further explore
		route, more := <- routes

		fmt.Println(route)

		// get the last explored coordinate
		lastexplored := route[len(route) - 1]

		// stop if we found the goal
		if lastexplored == goal {

			close(routes)
			return route, nil
		}

		if more {

			visited := make(map[Position]bool)

			for _, elem := range route {
				visited[elem] = true
			}

			richtingen := make(chan Position)
			go step(onceMaze, maze, lastexplored, visited, richtingen)

			for richting := range richtingen {

				new_route := route
				new_route = append(new_route, richting)

				routes <- new_route
			}

		} else {
			close(routes)
	        return nil, errors.New("incorrect input given, exiting.")
		}
	}

	close(routes)
	return nil,  errors.New("incorrect input given, exiting.")
}

func step(once [][]sync.Once, maze Maze, position Position,
		  visited map[Position]bool, richtingen chan Position) {

	row := position.Row
	col := position.Col

	boundsr := len(maze) - 1
	boundsc := len(maze[0]) - 1


	once[row][col].Do(func() {
		// Add to possible direction to the channel

		var pos byte = byte(maze[row][col])



		up := Position{row - 1, col}
		left := Position{row, col - 1}
		right := Position{row, col + 1}
		down :=  Position{row + 1, col}

		switch {

		case pos == 48:
			if col < boundsc && visited[right] == false {
				richtingen <- right
			}

			if row < boundsr && visited[down] == false {
				richtingen <- down
			}

		case pos == 49:

			if col < boundsr && visited[right] == false {
				richtingen <- right
			}

		case pos == 50:

			if col < boundsr && visited[down] == false {
				richtingen <- down
			}

		default:

		}

		if row != 0 {
			if ((maze[row - 1][col] == 48) || (maze[row - 1][col] == 50)) && visited[up] == false {
				richtingen <- up
			}
		}

		if col != 0 {
			if ((maze[row][col - 1]) == 48 || (maze[row][col - 1] == 49)) && visited[left] == false {
				richtingen <- left
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

	goal := Position {len(maze) - 1, len(maze[0]) - 1}
	route, maze_error := solve(maze, goal)

	if maze_error != nil {
		fmt.Println(maze_error)
		os.Exit(4)
	}

	for _, pos := range route {
		maze[pos.Row][pos.Col] |= byte(1 << 2) // The third flag
	}

	for _, line := range maze {
		fmt.Println(string(line))
	}
}
