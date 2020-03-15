package main

import (
	"fmt"
	"sync"
)

func main() {
	var once [11]sync.Once
	onceBody := func() {
		fmt.Println("Only once")
	}

	done := make(chan bool)

	for i := 0; i < 10; i++ {
		go func() {
			once[i].Do(onceBody)
			done <- true
		}()
	}

	for i := 0; i < 10; i++ {
		<-done
	}
}
