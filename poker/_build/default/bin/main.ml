open Deck
open Player
(* open Handscore *)
open Bot
type playerxscore = {player: returnplayer; score: int}


(* let rec main_game_loop (timesmax : int) (timesnow : int) : unit =
  if timesnow == timesmax then () else *)
  let () = print_endline "Welcome to OCamlPoker - by Otto Westerlund" 
  let play_game () : returnplayer list =
    let mydeck = createdeck2 [] 0 in
    let () = print_endline "Input how many players:" in
    let nplayers = read_int () in
    let retplayerlist = createAllPlayers nplayers 0 [] mydeck in
    let me = List.nth retplayerlist ((List.length retplayerlist)-1) in
    let meplayer = me.playerback in
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
      let botstr = string_of_int current in
      let botbetstr = string_of_int (List.nth asd current) in
        let output = "Bot " ^ botstr ^ " bets " ^ botbetstr in
      let () = print_endline output in
      print_bets asd (current+1) in

    let () = print_bets all_bot_bets 0 in
    let mycash = string_of_int meplayer.cash in
    let outstr = "You have " ^ mycash ^ " cash, enter bet:" in
    let () = print_endline outstr in
    let mybet = read_int () in
    let rec addlist (listin : int list) (cur : int) (sum : int) (listlen : int) : int =
      if cur == listlen then sum else
      let partsum = List.nth listin cur in
      addlist listin (cur+1) (sum+partsum) listlen in

    let pot = addlist (mybet::all_bot_bets) 0 0 nplayers in
    let () = meplayer.cash <- meplayer.cash-mybet in

    let calc_winner (players : returnplayer list) : returnplayer =
      let rec get_scores (playersIn : returnplayer list) (cur: int) (returnlist : playerxscore list) : playerxscore list  =
        if cur ==  nplayers then returnlist else
        let player = List.nth playersIn cur in
        let playerp = player.playerback in
        let score = Handscore.score_hand playerp.cards in
        let record = {player=player; score=score} in
        get_scores playersIn (cur+1) (record::returnlist) in
      let scores = get_scores players 0 [] in
      let scores_sorted = List.sort (fun x y -> compare (x.score) (y.score)) scores in
      let winnerxscore = List.nth scores_sorted (nplayers-1) in
      let winner = winnerxscore.player in
      winner in 
    
    let winner = calc_winner retplayerlist in
    if winner == me then
    let winstring = "YOU WIN (this hand) and you now have" ^ string_of_int (meplayer.cash+pot) ^ " cash" in
    let () = print_endline winstring in
    retplayerlist
    else let winstring = "Winner is Bot " ^ string_of_int (winner.playerback.id) in
    let () = print_endline winstring in
    retplayerlist
  let retplist = play_game ()
  let () = print_endline "Game over"
  let () = print_endline (string_of_int ((List.nth retplist 0).playerback.cash))
  (* if (List.nth retplist 0).playerback.cash > 0 then
  let () = print_endline "Play again!" in
  let () = main_game_loop timesmax (timesnow+1) in
   *)




  (* main_game_loop 3 0 *)