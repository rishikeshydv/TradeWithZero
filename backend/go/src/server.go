package main

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

func main() {
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

	bids := []Order{}
	asks := []Order{}

	users := []User{
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
