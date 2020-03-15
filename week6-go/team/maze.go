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

func solve(maze Maze, goal Position) (route []Position) {

	fmt.Println(maze)

	// fill once mace with correct amount of zeros??
	var onceMaze [][]sync.Once = make([][]int, len(maze))
	for i := range mat {
	    onceMaze[i] = make([]int, len(maze[0]))
	}

	// routes
	routes := make(chan []Position)

	// start the exploration at {0, 0}
	var explorestart [1]Position = Position{0, 0}

	// add the first route to the stack
	routes <- explorestart

	for {

		// gewandeld = matrix van dezelfde grootte als maze, maar met overal een 0
	    // (dat betekent: nog niet in dit hokje geweest).
	    // gewandeld[0,0] = 1 (dus in dit hokje zijn we wel geweest)
	    // locatie = (0,0)

		// Get the route to further explore
		route := <- routes

		// get the last explored coordinate
		lastexplored := route[len(route) - 1]

		// stop if we found the goal
		if lastexplored == goal {
			return route
		}

		richtingen := make(chan Position)
		go step(onceMaze, lastexplored, richtingen)

		for richting := range richtingen {
			routes <- append(route, richting)
	    }
	}

    // while de stack niet leeg is/ het kanaal open is:
    //     current gewandeld, current_locatie = neem de bovenste item van de stack
    //     richtingen = check_step(maze, current_locatie)
	//
    //     for elke richting in richtingen:
    //         if gewandeld[richting] == 0:
    //             nieuw_gewandeld = kopie(current_gewandeld)
    //             nieuw_gewandeld[richting] = 1
	//
    //             if richting == doel_locatie (nog even vaststellen hoe):
    //                 return nieuw_gewandeld (dit is je route)
    //             else:
    //                 voeg achteraan de stack toe {nieuw_gewandeld, richting

    return route
}

func step(maze Maze, position Position, richtingen chan Position) {

	row := position.Row
	col := position.Col

	// Add to possible direction to the channel
	switch maze[row][col] {

		case noWall:
			richtingen <- Position{row + 1, col}
	        richtingen <- Position{row, col + 1}

		case southWall:
			richtingen <- Position{row, col + 1}

		case eastWall:
			richtingen <- Position{row + 1, col}

		// there are no other cases (so no default required)
	}

    // if maze[row - 1, col] == 0 or maze[row -1, col] == 2 {
	// 	richtingen <- Position{row - 1, col} // we kunnen naar noord
	// }
    // else if maze[row, col -1] == 0 or maze[row, col - 1] == 1 {
	// 	richtingen <- Position{row, col - 1} // we kunnen naar west
	// }

	close(richtingen)
	return
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

	for _, pos := range solve(maze, goal) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		// TODO: handle errors
		fmt.Println(string(line))
	}
}
