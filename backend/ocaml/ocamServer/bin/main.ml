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



let users: user list = [
  { id: "1";balances: [("GOOGLE": 10),"USD": 50000]};
  { id: "2";balances: ["GOOGLE": 10,"USD": 50000]};
  ];