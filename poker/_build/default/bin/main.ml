open Deck
open Player
open Gamehelper






let () = print_endline "Welcome to OCamlPoker - by Otto Westerlund" 
let play_game () : returnplayer list =
  let mydeck = createdeck2 [] 0 in
  let () = print_endline "Input how many players (including yourself):" in
  let nplayers = read_int () in
  let retplayerlist = createAllPlayers nplayers 0 [] mydeck in

  let melist = List.filter (fun retplayer -> retplayer.playerback.bot == false) retplayerlist in
  
  let me = List.nth melist 0 in

  let meplayer = me.playerback in  
  
  let all_bot_bets = calc_bot_bets nplayers retplayerlist 0 [] in
  
  let rec addlist (listin : int list) (cur : int) (sum : int) (listlen : int) : int =
    if cur == listlen then sum else
    let partsum = List.nth listin cur in
    addlist listin (cur+1) (sum+partsum) listlen in
  
  let () = print_bets all_bot_bets 0 in
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
  let winstring = "YOU WIN (this hand) and you now have" ^ string_of_int (meplayer.cash+newpot) ^ " cash" in
  let () = print_endline winstring in
  let () = Gamehelper.show_cards bots 0 (List.length bots) in
  retplayerlist
  else let winstring = "Winner is Bot " ^ string_of_int (winner.playerback.id) in
  let () = print_endline winstring in
  let () = Gamehelper.show_cards bots 0 (List.length bots) in
  retplayerlist
 
 
let retplist = play_game ()
let () = print_endline "Game over"
let () = print_endline (string_of_int ((List.nth retplist 0).playerback.cash))


