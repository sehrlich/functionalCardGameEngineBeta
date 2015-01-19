open Core.Std
open List
open Str
open Array

type suit = Clubs | Hearts | Spades | Diamonds;; 
type zoneID = Deck | Active | Foundation of suit | Pile of int | HPile of int;; 
type card = { _suit:suit; _rank:int};; 

type orderedZone = card list;;
(*type full_board = Board of (card list) array;; *)

type choice = Quit | Choice of (card list array -> card list array);;

type moveType = Invalid | PileToPile of zoneID * zoneID | MultiPileToPile of int * zoneID  * zoneID | Meta ;;

(* val getSuitIndex : suit -> int *)
let suitByIndex s = match s with
                        | 0 -> Clubs    
                        | 1 -> Diamonds 
                        | 2 -> Spades   
                        | 3 -> Hearts ;;
(* val getSuitIndex : suit -> int *)
let getSuitIndex s = match s with
                        | Clubs     -> 0
                        | Diamonds  -> 1
                        | Spades    -> 2
                        | Hearts    -> 3 ;;

(* val getIndex : zoneId -> int *)
let getIndex zone = match zone with
                                | Deck -> 0
                                | Active -> 1
                                | Pile i when 1 <= i && i <= 7 -> i + 1
                                | HPile i when 1 <= i && i <= 7 -> i + 8
                                | Foundation s -> (getSuitIndex s) + 15;;

                                
(* val getPile : zoneId -> board -> card list *)
let getPile zone board =  board.(getIndex zone);;

(*diffColor :: Suit -> Suit -> Bool*)
let diffColor suit1 suit2 = match ( (getSuitIndex suit1) + (getSuitIndex suit2) ) mod 2 with
                        | 0 -> false
                        | 1 -> true;;

let string_of_card (c:card) = string_of_int c._rank ^ match c._suit with
                        | Clubs     -> "c"
                        | Diamonds  -> "d"
                        | Spades    -> "s"
                        | Hearts    -> "h" ;;
(*parseZone :: string -> zoneID option*)
(*should switch option with or_error*)
let parseZone s = match s with 
              | "d"       -> Some Deck
              | "deck"    -> Some Deck
              | "p1"      -> Some (Pile 1)
              | "p2"      -> Some (Pile 2)
              | "p3"      -> Some (Pile 3)
              | "p4"      -> Some (Pile 4)
              | "p5"      -> Some (Pile 5)
              | "p6"      -> Some (Pile 6)
              | "p7"      -> Some (Pile 7)
              | "1"       -> Some (Pile 1)
              | "2"       -> Some (Pile 2)
              | "3"       -> Some (Pile 3)
              | "4"       -> Some (Pile 4)
              | "5"       -> Some (Pile 5)
              | "6"       -> Some (Pile 6)
              | "7"       -> Some (Pile 7)
              | "cf"      -> Some (Foundation Clubs)
              | "df"      -> Some (Foundation Diamonds)
              | "hf"      -> Some (Foundation Hearts)
              | "sf"      -> Some (Foundation Spades)
              | "cF"      -> Some (Foundation Clubs)
              | "dF"      -> Some (Foundation Diamonds)
              | "hF"      -> Some (Foundation Hearts)
              | "sF"      -> Some (Foundation Spades)
              | _         -> None


(*val parseMove : string list -> moveType*)
let parseMove [meta] = match meta with 
                    | "quit" -> Meta
                    | "deck" -> PileToPile (Deck, Active)
                    | _   -> Invalid;;
let parseMove [fZ, tZ] =
        match (parseZone fZ, parseZone tZ) with
            | (_,None) -> Invalid
            | (None,_) -> Invalid
            | (Some f, Some t) -> PileToPile ((match f with | Deck -> Active | _ -> f) , t);;
(*
                
parseMove [n, fZ, tZ] =
        match (parseZone fZ, parseZone tZ) with
            | (_,None) -> Invalid
            | (None,_) -> Invalid
            | (Some f, Some t) -> MultiPileToPile 1; f ; t;;
            (*where num = read n:: Int*)

*)

let parseMove _string = Invalid;;


(*cardback :: String*)
(* cardback = "\ESC[104m()\ESC[0m" *)
let cardback = "()"

(*render :: Board -> ()*)
let render board = 
    (*let get s = Z.findWithDefault [] s board*)
    let topcard p = match getPile p board with 
                        | []            -> "__"
                        | (top::_rest)  -> string_of_card top in
    let deck = match (getPile Deck board) with 
                        | []            -> "__" 
                        | _             -> cardback in
    let pileString n =  "\nP" ^ string_of_int n ^ ":" 
                            ^ (String.concat ~sep:" " (List.map ~f:(fun _ -> cardback) (getPile (HPile n) board) ))
                            ^ (String.concat ~sep:" " @@ List.map ~f:string_of_card (List.rev @@ getPile (Pile n) board))
    in

    (*putStrLn "\ESC[H\ESC[2J"*)
    print_endline @@ "Deck:" ^ deck ^ "           " ^ topcard (Foundation Clubs)       ^ " " 
                                                    ^ topcard (Foundation Diamonds)    ^ " " 
                                                    ^ topcard (Foundation Hearts)      ^ " " 
                                                    ^ topcard (Foundation Spades);
    print_endline @@ "Active:" ^ (topcard Active);
    print_endline @@ pileString 1;
    print_endline @@ pileString 2;
    print_endline @@ pileString 3;
    print_endline @@ pileString 4;
    print_endline @@ pileString 5;
    print_endline @@ pileString 6;
    print_endline @@ pileString 7


(*klondikeForceMoveZone :: ZoneID -> ZoneID -> Board -> Board*)
let klondikeForceMoveZone fromZone toZone board =
    let fromInd = getIndex fromZone in
    let toInd   = getIndex toZone in
    match board.(fromInd) with
    | head :: tail -> 
            board.(fromInd) <- tail; 
            board.(toInd) <- (head :: board.(toInd));
    | [] -> ();; (* maybe an error *)


(*klondikeMove :: ZoneID -> ZoneID -> World -> World*)
let klondikeMove fromZone toZone board = 
            let fromInd = getIndex fromZone in
            let toInd   = getIndex toZone in
            if List.length (getPile fromZone board) = 0
            then
                match (fromZone, toZone) with
                | (Deck, Active) -> let curdeck = List.rev @@ board.(toInd) in
                    board.(fromInd) <- curdeck; board.(toInd) <- [];
                | _ -> () (* might raise error if being careful*)
            else
                let card = List.hd (getPile fromZone board) in
                match (fromZone, toZone) with
                | (HPile i, Pile j) when i=j 
                    -> klondikeForceMoveZone fromZone toZone board; ();
                | (_, Foundation s) 
                    -> ()
                    (* canPlace (Card _ s r) (Foundation f) _z b = f==s && r== if null foundationpile then 1 else _rank (head pile) + 1 *)
                | (_, Pile i)
                    -> ();
                    (* canPlace (Card _ s r) p@(Pile _i) _ b = if null pile then  r== 13 else (_rank (head xs) - 1) == r && diffColor s (_suit (head xs)) *)

            ;;
            (*
             * If fromZone was a pile, and is now empty
             * doe a hpile pile move
             *)



(*
(* klondikeMovePile :: Int -> ZoneID -> ZoneID -> World -> World *)
klondikeMovePile n fromID toID (World curBoard s)= 
                -- ASSUME THAT MOVE IS VALID
                -- THIS IS NOT A GOOD IDEA
                -- should work this back into regular klondikeMove
                
                let fromZone = fromJust $ Z.lookup fromID curBoard
                    chunk    = take n fromZone
                    newBoard = Z.adjust (drop n) fromID $ Z.adjust (chunk ++) toID curBoard
                in 
                    World newBoard s
*)

(*getInput :: IO Choice*)
let rec getInput = 
    (*putStrLn "Choose Move: "*)
    let mv = read_line () in
    match parseMove (Str.split (Str.regexp " ") mv) with
        | Invalid -> Quit
                (*print_endline "Invalid Move!" ; getInput*)
        | Meta    -> Quit 
        | PileToPile (fromZ, toZ)  -> Choice (fun b -> klondikeMove fromZ toZ b; b);;
        (*| MultiPileToPile num fromZ toZ  -> return $ Choice (liftM $ klondikeMovePile num fromZ toZ);;*)
        (*--  return $ Choice (liftM $ klondikeMove fromZ toZ)*)
        (*-- try taking reading two words as from and to (make sure limited to active, p# and *F) *)
        (*-- try reading card to zone*)

(*gameLoop :: IO World -> IO ()*)
let rec gameLoop board =
            render board;
            match getInput with
                    | Choice move -> gameLoop @@ move board
                    | Quit        -> return ();;



(*main :: IO ()*)
(*initialState :: IO World*)
let initialState board =  
    let stdDeck = ref [] in
    (*for i=1 to 52 do
        stdDeck := (Card (mod i 13)+1; suitByIndex (mod i 4)) :: stdDeck
    done*)
    board.(getIndex Deck) <- !stdDeck;
    (*shuffle stdDeck*)
    klondikeForceMoveZone Deck (Pile 1) board;
    klondikeForceMoveZone Deck (Pile 2) board;
    klondikeForceMoveZone Deck (Pile 3) board;
    klondikeForceMoveZone Deck (Pile 4) board;
    klondikeForceMoveZone Deck (Pile 5) board;
    klondikeForceMoveZone Deck (Pile 6) board;
    klondikeForceMoveZone Deck (Pile 7) board;
    (*(!!1) $ iterate (klondikeForceMoveZone Deck (HPile 2)) board*)
    (*(!!2) $ iterate (klondikeForceMoveZone Deck (HPile 3)) board*)
    (*(!!3) $ iterate (klondikeForceMoveZone Deck (HPile 4)) board*)
    (*(!!4) $ iterate (klondikeForceMoveZone Deck (HPile 5)) board*)
    (*(!!5) $ iterate (klondikeForceMoveZone Deck (HPile 6)) board*)
    (*(!!6) $ iterate (klondikeForceMoveZone Deck (HPile 7)) board*)
    board
in
let board = Array.create 20 ([]) in
gameLoop (initialState board)
