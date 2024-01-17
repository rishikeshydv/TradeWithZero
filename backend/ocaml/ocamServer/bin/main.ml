
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


let () =
  Dream.run
  @@ Dream.router [
    Dream.get "/echo/:word" @@ fun request ->
      Dream.html (Dream.param "word" request);
  ]

