package main

import (

	"fmt"
)

type Position struct {
	Row, Col int
}

func main() {

    routes := make(chan []Position)

	// start the exploration at {0, 0}
	explorestart := make([]Position, 0)
	explorestart = append(explorestart, Position{0, 0})

    fmt.Println(explorestart)

	// add the first route to the stack
	routes <- explorestart

    fmt.Println(<- routes)
    close(routes)

    return
}
