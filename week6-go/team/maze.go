package main

import (
	"bufio"
	"fmt"
	"os"
	"sync"
    "errors"
)

const noWall byte = (0 << 0) // The first flag
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
		maze = append(maze, []byte(s.Text()))
	}
	return maze, nil
}

func solve(maze Maze, goal Position) (route []Position, err error) {

	var new_route []Position
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
		fmt.Println(more)
		fmt.Println(route)

		// get the last explored coordinate
		lastexplored := route[len(route) - 1]
		fmt.Println(lastexplored)

		// stop if we found the goal
		if lastexplored == goal {
			close(routes)
			return route, nil
		}

		if more {
			richtingen := make(chan Position)

			go step(onceMaze, maze, lastexplored, richtingen)

			for richting := range richtingen {

				copy(new_route, route)
				new_route = append(new_route, richting)
				fmt.Println(new_route)
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
		  richtingen chan Position) {

	col := position.Col
	row := position.Row

	// zie hier: https://medium.com/golang-issue/how-singleton-pattern-works-with-golang-2fdd61cd5a7f

	once[col][row].Do(func() {

		// Add to possible direction to the channel
		if maze[row][col] & noWall == 0 {
			richtingen <- Position{row + 1, col}
			richtingen <- Position{row, col + 1}
		}

		if maze[row][col] & southWall != 0 {
			richtingen <- Position{row, col + 1}
		}

		if maze[row][col] & eastWall != 0 {
			richtingen <- Position{row + 1, col}
		}

		if col != 0 {
			if maze[row][col - 1] & noWall == 0 {
				richtingen <- Position{row, col - 1}
			}

			if maze[row][col - 1] & eastWall != 0 {
				richtingen <- Position{row, col - 1}
			}
		}

		if row != 0 {

			if maze[row - 1][col] & noWall == 0 {
				richtingen <- Position{row - 1, col}

			}

			if maze[row - 1][col] & eastWall != 0 {
				richtingen <- Position{row - 1, col}

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

	goal := Position {len(maze), len(maze[0])}
	route, maze_error := solve(maze, goal)

	if maze_error != nil {
		fmt.Println(maze_error)
		os.Exit(4)
	}

	for _, pos := range route {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		fmt.Println(string(line))
	}
}
