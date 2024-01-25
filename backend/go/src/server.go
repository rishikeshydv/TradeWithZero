package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"sort"

	"github.com/gorilla/mux"
)

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

type TypeAndQty struct {
	_type    string
	quantity int
}

type Aggregate map[int]TypeAndQty

var Symbol = "GOOGLE"

var bids = []Order{}
var asks = []Order{}

var users = []User{
	{id: "1",
		balances: Balances{
			"GOOGLE": 10,
			"USD":    500,
		},
	},
	{
		id: "2",
		balances: Balances{
			"GOOGLE": 20,
			"USD":    1000,
		},
	},
}

func main() {

	// Place a limit order
	http.HandleFunc("/order", func(w http.ResponseWriter, r *http.Request) {
		//set your content-type header so clients know to expect json
		//w.Header().Set("Content-Type", "application/json")

		var ordReq Request
		err := json.NewDecoder(r.Body).Decode(&ordReq)
		if err != nil {
			http.Error(w, "Invalid JSON request", http.StatusBadRequest)
			return
		}
		side := ordReq.side
		price := ordReq.price
		quantity := ordReq.quantity
		userId := ordReq.userId

		remainingQty := FillOrders(side, price, quantity, userId)

		if remainingQty == 0 {
			//data := { filledQuantity: quantity }
			//json.NewEncoder(w).Encode(data)
			fmt.Fprintf(w, "{ filledQuantity: %q }", quantity)
			return
		}

		if side == "bid" {
			bids = append(bids, []Order{
				userId:   userId,
				price:    price,
				quantity: remainingQty})

			sort.Slice(bids, func(i, j int) bool {
				return bids[i].price < bids[j].price
			})
		} else {
			asks = append(asks, []Order{
				userId:   userId,
				price:    price,
				quantity: remainingQty})

			sort.Slice(asks, func(i, j int) bool {
				return bids[i].price > bids[j].price
			})

		}
		fmt.Fprintf(w, "{ filledQuantity: %q }", quantity-remainingQty)
		return
	})

	// now we define /depth route
	http.HandleFunc("/depth", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		aggregateList := make(map[int]TypeAndQty)

		for i := 0; i < len(bids); i++ {
			agg, err := aggregateList[bids[i].price]
			if !err {
				aggregateList[bids[i].price] = TypeAndQty{
					_type:    "bid",
					quantity: bids[i].quantity,
				}
			} else {
				agg.quantity += bids[i].quantity
				aggregateList[bids[i].price] = agg

			}
		}

		for i := 0; i < len(asks); i++ {
			agg, err := aggregateList[asks[i].price]
			if !err {
				aggregateList[asks[i].price] = TypeAndQty{
					_type:    "sell",
					quantity: asks[i].quantity,
				}
			} else {
				agg.quantity += asks[i].quantity
				aggregateList[asks[i].price] = agg

			}
		}
		fmt.Fprintf(w, "Depth : %q", aggregateList)
	})

	http.HandleFunc("/balance/{userId}", GetBalanceHandler)

	http.HandleFunc("/quote", func(w http.ResponseWriter, r *http.Request) {
		// TODO: Assignment
	})

	http.ListenAndServe(":8080", nil)
}

// we will be using this function to extract userId from the URL
func GetBalanceHandler(w http.ResponseWriter, r *http.Request) {
	// Extract userId from the request path parameters
	vars := mux.Vars(r)
	userId := vars["userId"]

	for _, user := range users {
		if user.id == userId {
			// Set the json response content type
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprintf(w, "User Balance = %q", user.balances)
			return
		} else {
			fmt.Println("User Not Found!")
		}
	}

}

// this function updates the user stocks & bank balance
func UserUpdate(user1Id string, user2Id string, price int, quantity int, users []User) {
	var firstUser *User
	var secondUser *User

	for _, user := range users {
		if user.id == user1Id {
			firstUser = &user
			break
		} else if user.id == user2Id {
			secondUser = &user
		}
	}

	firstUser.balances[Symbol] -= quantity
	secondUser.balances[Symbol] += quantity
	firstUser.balances["USD"] += quantity * price
	secondUser.balances["USD"] -= quantity * price
}

// this function fills the bid or sell orders
func FillOrders(side string, price int, quantity int, userId string) int {

	remainingQty := quantity
	if side == "bid" {
		for i := len(asks) - 1; i >= 0; i-- {
			if asks[i].price > price {
				continue
			}

			if asks[i].quantity > remainingQty {
				asks[i].quantity -= remainingQty
				UserUpdate(asks[i].userId, userId, asks[i].price, remainingQty, users)
				return 0
			} else {
				remainingQty -= asks[i].quantity
				UserUpdate(asks[i].userId, userId, asks[i].price, asks[i].quantity, users)
				asks = asks[:len(asks)-1]
			}
		}
	} else {
		for i := len(bids) - 1; i >= 0; i-- {
			if bids[i].price < price {
				continue
			}

			if bids[i].quantity > remainingQty {
				bids[i].quantity -= remainingQty
				UserUpdate(userId, bids[i].userId, price, remainingQty, users)
				return 0
			} else {
				remainingQty -= bids[i].quantity
				UserUpdate(userId, bids[i].userId, bids[i].quantity, users)
				bids = bids[:len(asks)-1]
			}
		}
	}
	return remainingQty
}
