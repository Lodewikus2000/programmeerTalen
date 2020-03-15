package main

import (
	"fmt"
	"sync"
)

func main() {
    // nu gebeurt het 1x, want de de loop is maar tot 10
    //var once [11]sync.Once

    // nu gebeurt het 10x, want het is een dubbele array
    var once = make([][]sync.Once, 11)
    for i := range once {
    	once[i] = make([]sync.Once, 11)
	}

	onceBody := func() {
		fmt.Println("Only once")
	}

	done := make(chan bool)

    for j := 0; j < 10; j++ {
    	for i := 0; i < 10; i++ {
    		go func() {
    			once[j][i].Do(onceBody)
    			done <- true
    		}()
    	}
    }

    for j := 0; j < 10; j++ {
    	for i := 0; i < 10; i++ {
    		<-done
    	}
    }
}
