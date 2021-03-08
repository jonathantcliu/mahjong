module Tile exposing (..)
{-
  ( Tile,
  , orderedDeck
  , Hands
  , deal
  , dealAll
  , initDeck
  , showPlayerHand
  , tileToInt
  ) -}

type Value
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | North
  | South
  | East
  | West
  | Red
  | Green
  | White

type Suit
  = Dots
  | Bamboo
  | Characters
  | Winds
  | Dragons

type alias Tile =
  {value : Value, suit : Suit}

type alias Hands =
  { deck : List Tile
  , playerHand : List Tile
  , cpu1Hand : List Tile
  , cpu2Hand : List Tile
  , cpu3Hand : List Tile }

initDeck : List Tile
initDeck =
  orderedDeck

tileToString : Tile -> String
tileToString tile =
  (Debug.toString tile.value) ++ " " ++ (Debug.toString tile.suit)

tileToInt : Tile -> Int -- for sorting only
tileToInt tile =
  case tile.value of
    One ->
      1
    Two ->
      2
    Three ->
      3
    Four ->
      4
    Five ->
      5
    Six ->
      6
    Seven ->
      7
    Eight ->
      8
    Nine ->
      9
    East ->
      10
    South ->
      11
    West ->
      12
    North ->
      13
    Red ->
      14
    Green ->
      15
    White ->
      16

intToValue : Int -> Value
intToValue n =
  if n == 1 then One
  else if n == 2 then Two
  else if n == 3 then Three
  else if n == 4 then Four
  else if n == 5 then Five
  else if n == 6 then Six
  else if n == 7 then Seven
  else if n == 8 then Eight
  else if n == 9 then Nine
  else if n == 10 then East
  else if n == 11 then South
  else if n == 12 then West
  else if n == 13 then North
  else if n == 14 then Red
  else if n == 15 then Green
  else if n == 16 then White
  else Debug.todo "bad intToValue call"

collect : Suit -> List Tile -> List Tile
collect suit tiles =
  List.filter (\t -> t.suit == suit) tiles

remove : a -> List a -> List a
remove x xs =
  case xs of
    [] ->
      []
    first::rest ->
      if first == x then rest
      else first :: remove x rest

zipFour : List Suit -> List Value -> List Tile
zipFour suits values =
  List.concat
    (List.concatMap
      (\value ->
        List.map (\suit -> List.repeat 4 (Tile value suit)) suits) values)

orderedDeck : List Tile
orderedDeck =
  let
    values =
      [ One,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine
      ]
    suits =
      [ Dots,
        Bamboo,
        Characters
      ]
    directions =
      [ North,
        South,
        East,
        West
      ]
    wind = [Winds]
    colors =
      [ Red,
        Green,
        White
      ]
    dragon = [Dragons]
  in
    zipFour suits values ++ zipFour wind directions ++ zipFour dragon colors

dealAll : List Tile -> Hands
dealAll deck =
  let
    (playerHand, d2) = deal [] deck 13
    (cpu1Hand, d3) = deal [] d2 13
    (cpu2Hand, d4) = deal [] d3 13
    (cpu3Hand, d5) = deal [] d4 13
  in
    Hands d5 playerHand cpu1Hand cpu2Hand cpu3Hand

deal : List Tile -> List Tile -> Int -> (List Tile, List Tile)
deal to from n =
  let
    hand = List.take n from
    rest = List.drop n from
  in
    (to ++ hand, rest)

showPlayerHand : List Tile -> List String
showPlayerHand hand =
  List.map (\t -> stringToGlyph (tileToString t)) hand

showCPUHand : List Tile -> List String
showCPUHand hand =
  List.map (\t -> "🀫") hand

sortHand : List Tile -> List Tile
sortHand hand =
  let
    dots = collect Dots hand
    bamboo = collect Bamboo hand
    characters = collect Characters hand
    winds = collect Winds hand
    dragons = collect Dragons hand
  in
    sortNumbered dots ++ sortNumbered bamboo ++ sortNumbered characters ++
    sortNumbered winds ++ sortNumbered dragons

sortNumbered : List Tile -> List Tile
sortNumbered hand =
  let
    tile = List.head hand
    numbers = List.map tileToInt hand
    sorted = List.sort numbers
  in
    case tile of
      Nothing ->
        []
      Just t ->
        List.map (\n -> Tile (intToValue n) t.suit) sorted

stringToGlyph : String -> String
stringToGlyph string =
  case string of
    "One Dots" ->
      "🀙"
    "Two Dots" ->
      "🀚"
    "Three Dots" ->
      "🀛"
    "Four Dots" ->
      "🀜"
    "Five Dots" ->
      "🀝"
    "Six Dots" ->
      "🀞"
    "Seven Dots" ->
      "🀟"
    "Eight Dots" ->
      "🀠"
    "Nine Dots" ->
      "🀡"
    "One Bamboo" ->
      "🀐"
    "Two Bamboo" ->
      "🀑"
    "Three Bamboo" ->
      "🀒"
    "Four Bamboo" ->
      "🀓"
    "Five Bamboo" ->
      "🀔"
    "Six Bamboo" ->
      "🀕"
    "Seven Bamboo" ->
      "🀖"
    "Eight Bamboo" ->
      "🀗"
    "Nine Bamboo" ->
      "🀘"
    "One Characters" ->
      "🀇"
    "Two Characters" ->
      "🀈"
    "Three Characters" ->
      "🀉"
    "Four Characters" ->
      "🀊"
    "Five Characters" ->
      "🀋"
    "Six Characters" ->
      "🀌"
    "Seven Characters" ->
      "🀍"
    "Eight Characters" ->
      "🀎"
    "Nine Characters" ->
      "🀏"
    "North Winds" ->
      "🀃"
    "South Winds" ->
      "🀁"
    "East Winds" ->
      "🀀"
    "West Winds" ->
      "🀂"
    "Red Dragons" ->
      "🀄"
    "Green Dragons" ->
      "🀅"
    "White Dragons" ->
      "🀆"
    _ ->
      ""
