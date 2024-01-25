package main

import "fmt"

type Balances map[string]int

type User struct {
	id       string
	balances Balances
}

type Order struct {
	userId   string
	price    int
	quantity int
}

var Symbol = "GOOGLE"

func main(
	fmt.Println("Hello")
)