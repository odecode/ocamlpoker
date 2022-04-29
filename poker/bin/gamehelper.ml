open Player
open Bot

type playerxscore = {player: returnplayer; score: int}

let rec calc_bot_bets (num_players : int) (players : returnplayer list) (curbot_index : int) (returnlist : int list) : int list =
  if num_players-1 == curbot_index then returnlist else
  let currentbotretplayer = List.nth players curbot_index in
  let currentbot = currentbotretplayer.playerback in
  (* let bothand = currentbot.cards in *)
  let bot_bet_result = bot_bet currentbot in
  let betstr = "Bot " ^ string_of_int currentbot.id ^ " bets " ^ string_of_int bot_bet_result in
  let () = print_endline betstr in
  let () = currentbot.cash <- currentbot.cash - bot_bet_result in
  calc_bot_bets num_players players (curbot_index+1) (bot_bet_result::returnlist)

let rec print_bets (asd : int list) (current : int) : unit =
  let len = List.length asd in
  if current == len then () else
  let botstr = string_of_int current in
  let botbetstr = string_of_int (List.nth asd current) in
    let output = "Bot " ^ botstr ^ " bets " ^ botbetstr in
  let () = print_endline output in
  print_bets asd (current+1) 

let calc_winner (nplayers : int) (players : returnplayer list) : returnplayer =
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
  winner

let rec show_cards (bots : returnplayer list) (cur : int) (listlen : int) : unit =
  if cur == listlen then () else
  let retbot = List.nth bots cur in
  let bot = retbot.playerback in
  let cards = bot.cards in
  let botstr = "Bot " ^ string_of_int (bot.id) ^ " cards:" in
  let () = print_endline botstr in
  let () = Deck.print_deck cards 0 in
  show_cards bots (cur+1) listlen