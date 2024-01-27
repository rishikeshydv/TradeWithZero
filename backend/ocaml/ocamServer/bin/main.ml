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

type Request = {
  side: string;
  price: number;
  quantity: number;
  userId: string;
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

(* This is a helper function to append to the list *)
let listPush (orderList: order list)(userId:string)(price:int)(quantity:int) = 
  match orderList with
  |[] :List.rev_append orderList [{ id = userId; price = price; quantity = quantity }];
  |hd:tl -> 
    let newOrder: order = { id = userId; price = price; quantity = quantity } in
    List.rev_append (newOrder :: orderList);;


  (* This is a pattern matching for sorting asks based on price *)
  let sortAsk (order1:order) (order2:order) =
    match order1.price, order2.price with
    |a,b when a>=b -> 1;
    |a,b when a<b -> -1;
  ;; 

  (* This is a pattern matching for sorting bids based on price *)
  let sortBid (order1:order) (order2:order) =
    match order1.price, order2.price with
    |a,b when a<=b -> 1;
    |a,b when a>b -> -1;
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

(*Here we will be writing the routes*)

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/order" 
    (fun (request:Request) ->
      let side string = request.side in
      let price number = request.price in
      let quantity number = request.quantity in
      let userId string = request.userId in

      let _remainingQuantity = fillOrders side price quantity userId in

      if _remainingQuantity = 0 then
        begin
        let response = { filledQuantity = quantity } in
        Dream.json response
        end
      else
        Dream.respond (Dream.status `OK) "Order received";
      ;;

      if side = "bid" then
        begin
        listPush(bids;userId,price;quantity);
        List.sort sortBid bids;
        end
      else
        begin
          listPush(asks,userId,price,quantity);
          List.sort sortAsk asks;
        end
      let response = { filledQuantity = quantity - _remainingQuantity } in
      Dream.json response;
      );;
  ]

  if (side === "bid") {
    bids.push({
      userId,
      price,
      quantity: remainingQty
    });
    bids.sort((a, b) => a.price < b.price ? -1 : 1);
  } else {
    asks.push({
      userId,
      price,
      quantity: remainingQty
    })
    asks.sort((a, b) => a.price < b.price ? 1 : -1);
  }


