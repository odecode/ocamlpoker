open Deck
open Player

let () = print_endline "Hello, World!"
let () = print_endline "Input how many players:"
let nplayers = read_int ()
let nplayersstr = string_of_int nplayers
let () = print_endline nplayersstr



let mydeck = createdeck ([]) (0)


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

let newplayerrecord = createplayer (mydeck)
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