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

    /*
        When we use ... instead of specifying the length.
        The compiler can identify the length of an array,
        based on the elements specified in the array declaration.
     */

	explorestart := []Position {Position{0, 0}}

    fmt.Println(explorestart)

	// add the first route to the stack
	routes <- explorestart

    fmt.Println(<- routes)
    close(routes)

    return
}
