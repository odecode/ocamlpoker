open Deck
open Player

let () = print_endline "Hello, World!"
let () = print_endline "Input how many players:"

let nplayers = read_int ()
let nplayersstr = string_of_int nplayers


let () = print_endline nplayersstr



let mydeck = createdeck ([]) (0)

let retplayerlist = createAllPlayers nplayers 0 [] mydeck

let meret = List.nth retplayerlist (List.length (retplayerlist) -1)
let me = meret.playerback
let bot = List.nth retplayerlist 0
let mydeck = bot.deckback

let listlen = List.length mydeck
let lstr = string_of_int listlen

let () = print_endline "Deck length after creating players"
let () = print_endline lstr

let mycard = List.nth me.cards 0
let () = print_card mycard













(*
   
let () = print_endline "Next"
let acard = draw_random_card (mydeck)
let () = print_card (acard)
let l = List.length mydeck
let ls = string_of_int l
let () = print_endline ls

let () = print_endline "Orig deck length"
let l = List.length mydeck
let ls = string_of_int l
let () = print_endline ls

let newplayerrecord = createplayer (mydeck) (false)
let newplayer = newplayerrecord.playerback
let newdeck = newplayerrecord.deckback

let cardlist = newplayer.cards
let pcard = List.nth cardlist 4
let () = print_card pcard

let () = print_endline "Deck len after"
let l = List.length newdeck
let ls = string_of_int l
let () = print_endline ls

let () = print_endline "Player cards len"
let l = List.length cardlist
let ls = string_of_int l
let () = print_endline ls

let pot = 50

let pcash = newplayer.cash
let cashstr = string_of_int pcash
let () = print_endline "Player cash"
let () = print_endline cashstr

let back = bet_cash (87) (pot) (pcash)
let pcash = back.cashleft


let cashstr = string_of_int pcash
let () = print_endline "Player cash"
let () = print_endline cashstr
*)