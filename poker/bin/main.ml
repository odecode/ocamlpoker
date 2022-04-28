open Deck
open Player
(* open Handscore *)
open Bot

(* let rec main_game_loop (timesmax : int) (timesnow : int) : unit =
  if timesnow == timesmax then () else *)
  let () = print_endline "Welcome to OCamlPoker - by Otto Westerlund" 
  let play_game () : returnplayer list =
    let mydeck = createdeck2 [] 0 in
    let () = print_endline "Input how many players:" in
    let nplayers = read_int () in
    let retplayerlist = createAllPlayers nplayers 0 [] mydeck in
    let rec calc_bot_bets (num_players : int) (players : returnplayer list) (curbot_index : int) (returnlist : int list) : int list =
      if num_players == curbot_index then returnlist else
      let currentbotretplayer = List.nth players curbot_index in
      let currentbot = currentbotretplayer.playerback in
      (* let bothand = currentbot.cards in *)
      let bot_bet_result = bot_bet currentbot in
      let () = currentbot.cash <- currentbot.cash - bot_bet_result in
      calc_bot_bets num_players players (curbot_index+1) (bot_bet_result::returnlist) in
    
    let all_bot_bets = calc_bot_bets nplayers retplayerlist 0 [] in
    
    let rec print_bets (asd : int list) (current : int) : unit =
      let len = List.length asd in
      if current == len then () else
      let () = print_endline (string_of_int (List.nth asd current)) in
      print_bets asd (current+1) in

    let () = print_bets all_bot_bets 0 in

    retplayerlist
  let retplist = play_game ()
  let () = print_endline "Game over"
  let () = print_endline (string_of_int ((List.nth retplist 0).playerback.cash))
  (* if (List.nth retplist 0).playerback.cash > 0 then
  let () = print_endline "Play again!" in
  let () = main_game_loop timesmax (timesnow+1) in
   *)




  (* main_game_loop 3 0 *)