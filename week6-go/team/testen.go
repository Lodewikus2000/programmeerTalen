package main
import (

    "fmt"
)

var done chan int = make(chan int)
func hello() {
fmt.Println("Hello World!")
done <- 1 // The 1 carries no meaning
done <- 2
}
func main() {
// Spawn two goroutines
go hello()
go hello()
go hello()
// Wait for them to finish
<-done
<-done
<-done
}
