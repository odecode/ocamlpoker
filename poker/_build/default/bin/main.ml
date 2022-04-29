open Deck
open Player
open Gamehelper






let () = print_endline "Welcome to OCamlPoker - by Otto Westerlund" 
let rec play_game (cashlist : cashrecord list) (nplayers : int) : int =
  let mydeck = createdeck2 [] 0 in
  
  let retplayerlist = if (List.length cashlist) == 0 then createAllPlayers nplayers 0 [] mydeck
  else createAllPlayersWithCash cashlist nplayers 0 [] mydeck in

  let melist = List.filter (fun retplayer -> retplayer.playerback.bot == false) retplayerlist in
  
  let me = List.nth melist 0 in

  let meplayer = me.playerback in  
  
  let all_bot_bets = calc_bot_bets nplayers retplayerlist 0 [] in
  
  let rec addlist (listin : int list) (cur : int) (sum : int) (listlen : int) : int =
    if cur == listlen then sum else
    let partsum = List.nth listin cur in
    addlist listin (cur+1) (sum+partsum) listlen in
  
  (* let () = print_bets all_bot_bets 0 in *)
  let pot = addlist (all_bot_bets) 0 0 (nplayers-1) in  
  let potstr = "Pot is " ^ (string_of_int pot) in
  let () = print_endline potstr in

  let () = print_endline "Your cards are:" in

  let () = print_deck meplayer.cards 0 in

  let mycash = string_of_int meplayer.cash in
  let outstr = "You have " ^ mycash ^ " cash, enter bet:" in
  let () = print_endline outstr in
  let mybet = read_int () in

  let newpot = addlist (mybet::all_bot_bets) 0 0 nplayers in

  let potstr = "Pot is " ^ (string_of_int newpot) in

  let () = print_endline potstr in

  let () = meplayer.cash <- meplayer.cash-mybet in

  let bots = List.filter (fun retplayer -> retplayer.playerback.bot == true) retplayerlist in
  
  let winner = Gamehelper.calc_winner nplayers retplayerlist in
  if winner == me then
  let () = meplayer.cash <- meplayer.cash+newpot in
  let cashfornextgame = Gamehelper.make_cashlist retplayerlist 0 [] in
  let winstring = "YOU WIN (this hand) and you now have " ^ string_of_int (meplayer.cash) ^ " cash" in
  let () = print_endline winstring in
  let () = Gamehelper.show_cards bots 0 (List.length bots) in
  let () = print_endline "Continue? (1 for continue, any number for quit)" in
  let continue = read_int () in
  if continue == 1 && meplayer.cash > 0 then play_game (cashfornextgame) nplayers else
  meplayer.cash
  
  else let winstring = "Winner is Bot " ^ string_of_int (winner.playerback.id) ^ " and you now have " ^ string_of_int meplayer.cash ^ " cash" in
  let cashfornextgame = Gamehelper.make_cashlist retplayerlist 0 [] in
  let () = print_endline winstring in
  let () = Gamehelper.show_cards bots 0 (List.length bots) in
  let () = print_endline "Continue? (1 for continue, any number for quit)" in
  let continue = read_int () in
  if continue == 1 && meplayer.cash > 0 then play_game (cashfornextgame) nplayers else
  meplayer.cash
 


let () = print_endline "Input how many players (including yourself):" 
let nplayers = read_int ()
let finalcash = play_game ([]) nplayers
let byestring = "Game over, you walk away with " ^ string_of_int finalcash ^ " cash"
let () = print_endline byestring


