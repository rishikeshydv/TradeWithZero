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