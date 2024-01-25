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
}

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

func FillOrders() {

}
