(* defining types *)

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


(* User Retrieval*)

let users: user list = [
  { id: "1";balances: ["GOOGLE": 10,"USD": 50000]};
  { id: "2";balances: ["GOOGLE": 10,"USD": 50000]};
];;


(* defining bids and asks *)

let bids: order list = [];;
let asks: order list = [];;

let TICKER = "GOOGLE";;


(* Here, we will be writing helper functions to deal with immutability *)
let _increaseBalance = func increaseBalance (_balance:balances)(_price:int)(_quantity:int) ->
  let rec recurseBalanceHelper acc = function 
  (*pattern matching*)
  |[] -> List.rev((TICKER,_quantity)::("USD",_price*_quantity)::acc)
  |(t,q)::("USD",p)::tl when t = TICKER -> List.rev_append((t,q-_quantity)::("USD",p+_price*_quantity)::tl)
  |hd::tl -> recurseBalanceHelper (hd :: acc) tl
in recurseBalanceHelper [] _balance;;   (*this is the starting point*)

let _decreaseBalance = func decreaseBalance (_balance:balances)(_price:int)(_quantity:int) ->
  let rec recurseBalanceHelper acc = function 
  (*pattern matching*)
  |[] -> List.rev((TICKER,_quantity)::("USD",_price*_quantity)::acc)
  |(t,q)::("USD",p)::tl when t = TICKER -> List.rev_append((t,q+_quantity)::("USD",p-_price*_quantity)::tl)
  |hd::tl -> recurseBalanceHelper (hd :: acc) tl
in recurseBalanceHelper [] _balance;;   (*this is the starting point*)

(*here we create the entire new user with updated credentials*)
let _increaseUser = func increaseUser (_user:user)(_balance:balances)(_price:int)(_quantity:int) ->
  {_user with balance = _increaseBalance _balance _price _quantity}
;;

let _decreaseUser = func decreaseUser (_user:user)(_balance:balances)(_price:int)(_quantity:int) ->
  {_user with balance = _decreaseBalance _balance _price _quantity}
;;

(* defining a balance flipping function that flips stock and money of buyer and seller *)
let fB = fun flipBalance (userId1: string) (userId2: string) (quantity: number) (price: number) ->

  let user1 = List.find(fun u -> u.id=userId1) users in
  let user2 = List.find(fun u -> u.id = userId2.id) users in

  (* here we will be defining an action to be taken when users are not found *)
  let () : unit = if not user1 && not user2 then
      () ;;

    _decreaseUser user1 user1.balance price quantity
    _increaseBalance user2 user2.balance price quantity
;;




let fO = fun fillOrders side:string price:number quantity:number userId:string ->
  let remainingQuantity = quantity;;
if side = "bid" then
  (* 
function fillOrders(side: string, price: number, quantity: number, userId: string): number {
  let remainingQuantity = quantity; *)
  if (side === "bid") {
      for (let i = asks.length - 1; i >= 0; i--) {
          if (asks[i].price > price) {
              continue;
            }
              if (asks[i].quantity > remainingQuantity) {
                  asks[i].quantity -= remainingQuantity;
                  flipBalance(asks[i].userId, userId, remainingQuantity, asks[i].price);
                  return 0;
                } else {
  remainingQuantity -= asks[i].quantity;
  flipBalance(asks[i].userId, userId, asks[i].quantity, asks[i].price);
  asks.pop();
}
}
} else {
  for (let i = bids.length - 1; i >= 0; i--) {
      if (bids[i].price < price) {
          continue;
        }
          if (bids[i].quantity > remainingQuantity) {
              bids[i].quantity -= remainingQuantity;
              flipBalance(userId, bids[i].userId, remainingQuantity, price);
              return 0;
            } else {
  remainingQuantity -= bids[i].quantity;
  flipBalance(userId, bids[i].userId, bids[i].quantity, price);
  bids.pop();
}
}
}

return remainingQuantity;
}




let () =
  Dream.run
  @@ Dream.router [
    Dream.post "/order" @@ fun request ->
    Dream.html (Dream.param "word" request);
  ]



