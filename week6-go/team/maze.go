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

func solve(maze Maze, goal Position) (route []Position) {

	// initialize 2D onceMaze of equivalent size as the input maze
	var onceMaze = make([][]sync.Once, len(maze) + 1)
	for i := range onceMaze {
    	onceMaze[i] = make([]sync.Once, len(maze[0]) + 1)
	}

	// make a channel to save routes
	routes := make(chan []Position)

	// Acknowledgement channel
	ack := make(chan bool)

	// Done channel
	done := make(chan bool)

	boundsr := len(maze) - 1
	boundsc := len(maze[0]) - 1

	go start_exploration(routes)

	// count := 1
	//
	// for count != 0 {
	//
	// 	// fmt.Println(count)
	//
	// 	select {
	//
	//
	// 	case <-done:
	// 		// delete last element
	// 		count -= 1
	// 		route = route[:len(route) - 1]
	//
	// 	case route := <-routes:
	//
	// 		fmt.Println(route)
	//
	// 		last := route[len(route) - 1]
	//
	// 		if last == goal {
	// 			return route
	// 		}
	//
	// 		row := last.Row
	// 		col := last.Col
	//
	//

	// go onceMaze[row][col].Do(func() {
	for {
		go func() {

			route := <-routes

			fmt.Println(route)

			last := route[len(route) - 1]

			row := last.Row
			col := last.Col

			// Add to possible direction to the channel

			var pos byte = byte(maze[row][col])

			up := append(route, Position{row - 1, col})
			left := append(route, Position{row, col - 1})
			right := append(route, Position{row, col + 1})
			down := append(route, Position{row + 1, col})

			switch {

			case pos == 48:
				if col < boundsc {
					routes <- up
					ack <- true
				}

				if row < boundsr {
					routes <- down
					ack <- true
				}

			case pos == 49:

				if col < boundsc {
					routes <- right
					ack <- true
				}

			case pos == 50:

				if col < boundsr {
					routes <- down
					ack <- true
				}

			default:

			if row != 0 {
				if ((maze[row - 1][col] == 48) ||
					(maze[row - 1][col] == 50)) {

					routes <- up
					ack <- true
				}
			}

			if col != 0 {
				if ((maze[row][col - 1]) == 48 ||
					(maze[row][col - 1] == 49)) {

					routes <-  left
					ack <- true
				}
			}
			}

			done <- true
		}()

		fmt.Println(<-routes)
		fmt.Println(<-ack)
		fmt.Println(<-done)
	}
	// 		count++
	//
	// 		if ! <- ack {
	//
	// 		} else {
	//
	// 			close(ack)
	// 			close(done)
	// 			close(routes)
	// 			return nil
	// 		}
	//
	// 	}
	// }


	return route
}

func start_exploration(routes chan []Position){
	startexplore := make([]Position, 0)
	startexplore = append(startexplore, Position{0, 0})

	fmt.Printf("%T\n", routes)
	fmt.Printf("%T\n", startexplore)

	routes <- startexplore
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
	// route, maze_error := solve(maze, goal)
	//
	// if maze_error != nil {
	// 	fmt.Println(maze_error)
	// 	os.Exit(4)
	// }

	for _, pos := range solve(maze, goal) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		fmt.Println(string(line))
	}
}
