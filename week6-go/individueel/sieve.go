/*
 * Leo Schreuders
 * Programmeertalen
 * 11 maart 2020
 * Implementt a concurrent sieve for filtering primes for a range.
 *
 * Received help by Pim van Helvoirt
*/

package main

import (
  "fmt"
  "os"
  "strconv"
)


func genNum(limit int, channel chan int) {
  for i := 2; i <= limit; i++ {
    channel <- i
  }
  close(channel)
}

func filter(src chan int, dest chan int, prime int) {
  for num := range src {
    if num % prime != 0 {
      dest <- num
    }
  }
  close(dest)
}


func sieve (channel chan int, limit int, done chan bool) {

  go genNum(limit, channel)

  for i := 0; i < limit; i++ {
      prime, more := <-channel

      if more {
        fmt.Println(prime)

        new_channel := make(chan int)
        go filter(channel, new_channel, prime)

        channel = new_channel
      } else {
        done <- true
        return
      }
  }
}


func main() {

  if len(os.Args) != 2 {
    fmt.Println("incorrect number of arguments")
    os.Exit(2)
  }

  string := os.Args[1]

  limit, error := strconv.Atoi(string)

  if error != nil {
    fmt.Println(error)
    os.Exit(3)
  }

  done := make(chan bool)
  channel := make(chan int)


  go sieve(channel, limit, done)

  <-done

  close(done)

}
