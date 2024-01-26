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
let _flipBalance = fun flipBalance (userId1: string) (userId2: string) (quantity: number) (price: number) ->

  let user1 = List.find(fun u -> u.id=userId1) users in
  let user2 = List.find(fun u -> u.id = userId2.id) users in

  (* here we will be defining an action to be taken when users are not found *)
  let () : unit = if not user1 && not user2 then
      () ;;

    _decreaseUser user1 user1.balance price quantity;
    _increaseBalance user2 user2.balance price quantity;
;;

(*Here, we will write a pattern matching to retrieve 
asks[i] as OCaml doesnt allow direct list access

This is a helper function for FillOrders function
   *)
let rec listAccess n:int transactList:List =
  match transactList with
  |[] -> failwith "List index out of bounds"
  |hd::tl -> if n=0 then hd else listAccess n-1 tl ;;

  (* This is the helper function for updating asks[i].quantity *)
let updateAskQuantity(orderList:order list)(index:int)(quantity:int):order list = 
  let updateHelper idx,qty acc = function 
  |[] -> failwith "No Records Found"
  |hd::tail -> if idx = 0 ->
    let updatedQty = max 0 (hd.quantity - quantity) in
    let updatedUser = hd with quantity = updatedQty in (* Here, hd is existing orderList and its quantity attribute is updated with 'updatedQty' *)
    List.rev_append (updatedUser::acc) tl in
  |hd::tail -> updateHelper idx-1 qty (hd::acc) tl
  in updateHelper index quantity [] asks;;

    (* This is the helper function for updating bids[i].quantity *)
let updateBidQuantity(orderList:order list)(index:int)(quantity:int):order list = 
let updateHelper idx,qty acc = function 
|[] -> failwith "No Records Found"
|hd::tail -> if idx = 0 ->
  let updatedQty = max 0 (hd.quantity - quantity) in
  let updatedUser = hd with quantity = updatedQty in (* Here, hd is existing orderList and its quantity attribute is updated with 'updatedQty' *)
  List.rev_append (updatedUser::acc) tl in
|hd::tail -> updateHelper idx-1 qty (hd::acc) tl
in updateHelper index quantity [] bids;;

(* This is the helper function to update remaining quantity*)
let _remainingQty (originalQuantity:int transactionQuantity:int) :int = 
  match originalQuantity with
  |0 -> failwith "No quantity left"
  |num when num > 0 ->
    let updatedQuantity = transactionQuantity - originalQuantity in
    updatedQuantity
  |_ -> failwith "Invalid Input"
;; 

(* This is a helper pattern matching function to pop the last element of a list *)
let rec listPop (orderList: order list) :order list =
  match orderList with
  |[] -> failwith "Empty List Found"
  |[_]->[] (* When the list only has one element *)
  |hd::tl -> hd:: listPop tl
;;

(*Here we will define fillOrders function*)
let _fillOrders = fun fillOrders (side:string) (price:number) (quantity:number) (userId:string) :int ->
  let remainingQuantity = quantity in
  if side = "bid" then
    begin
    let askLen = List.length asks in
    for i = askLen - 1 to -1 () do
      let askPrice = listAccess i asks in
      if askPrice.price > price then
        ()
      else
        begin
        if askPrice.quantity > remainingQuantity then
          begin
          _flipBalance askPrice.userId userId askPrice.price quantity;
          updateAskQuantity(asks i quantity);
          end
        else
          begin
          _remainingQty remainingQuantity askPrice.quantity;
          _flipBalance askPrice.userId userId price askPrice.quantity;
          listPop asks;
          end
    done
  end
  else
    begin
    let bidLen = List.length bids in
    for i = bidLen - 1 to -1 () do
      let bidPrice = listAccess i bids in
      if bidPrice.price > price then
        ()
      else
        begin
        if bidPrice.quantity > remainingQuantity then
          begin
          _flipBalance bidPrice.userId userId bidPrice.price quantity;
          updateBidQuantity bids i quantity;
          end
        else
          begin
          _remainingQty remainingQuantity bidPrice.quantity;
          _flipBalance bidPrice.userId userId price bidPrice.quantity;
          end
    done 
  end
  remainingQuantity;;

let () =
  Dream.run
  @@ Dream.router [
    Dream.post "/order" @@ fun request ->
    Dream.html (Dream.param "word" request);
  ]



