package main

import (
	"bufio"
	"fmt"
	"os"
	// "sync"
    "errors"
)

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

func solve(maze Maze) (route []Position) {
	fmt.Println(maze)

	// Receive messages from the goroutines and spawn new ones as needed
	// Do not spawn new goroutines if a way out of the maze has been found
	// Stop receiving only when no more exploration goroutines are running

    return route
}

func step(maze Maze, position Position) {
	richtingen := make(chan []Position)

	row int = Position.Row
	col int = Position.Col
	if maze[row, col] == 0:
        richtingen <- Position{row + 1, col} // we kunnen naar zuid
        richtingen <- Position{row, col + 1} // we kunnen naar oost
    else if maze[row, col] == 1:
        richtingen <- Position{row, col + 1} // we kunnen naar oost
    else if maze[row, col] == 2:
        richtingen <- Position{row + 1, col} // we kunnen naar zuid

    if maze[row - 1, col] == 0 of 2:
        richtingen <- Position{row - 1, col} // we kunnen naar noord
    if maze[row, col -1] == 0 of 1:
        richtingen <- Position{row, col - 1} // we kunnen naar west

	close(richtingen)
	return richtingen
}

func main() {
	// TODO: handle errors
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

	for _, pos := range solve(maze) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		// TODO: handle errors
		fmt.Println(string(line))
	}
}
