
//defining types

type balances = (string * int) list;;

type user = {
  id: string;
  balances: balances;
};;

type order = {
  userId: string;
  price: int;
  quantity: int;
};;


//user retrieval

let users: user list = [
  { id: "1";balances: [("GOOGLE": 10),"USD": 50000]};
  { id: "2";balances: ["GOOGLE": 10,"USD": 50000]};
];;


//defining bids and asks
let bids list = [];;
let asks list = [];;

let TICKER = "GOOGLE";;

//defining a balance flipping function that flips stock and money of buyer and seller

let fB = fun flipBalance userId1: string userId2: string quantity: number price: number ->
let user1 = List.find(fun u -> u.id=userId1) users in
let user2 = List.find(fun u -> u.id = userId2.id) users in

//here we will be defining an action to be taken when users are not found
let () : unit = if not user1 && not user2 then
  () ;;

user1.balances[TICKER] += quantity;;
user2.balances[TICKER] -= quantity;;
user1.balances['USD'] += quantity*price;;
user2.balances['USD'] -= quantity*price;;

;;





let () =
  Dream.run
  @@ Dream.router [
    Dream.post "/order" @@ fun request ->
      Dream.html (Dream.param "word" request);
  ]
  


