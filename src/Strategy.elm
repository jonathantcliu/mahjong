module Strategy exposing (..)

import Tile exposing (Tile, Suit(..), Value(..))


type Attempt
  = Hu (List Tile) -- winning hand!
  | Gang (List Tile, List Tile) -- (meld, leftover tiles)
  | Peng (List Tile, List Tile) -- (meld, leftover tiles)
  | Chi (List Tile, List Tile) -- (meld, leftover tiles)

countTiles : List Tile -> Tile -> Int
countTiles tiles newtile =
  let
    loop l t seen =
      case l of
        [] ->
          seen
        first::rest ->
          if first == t then loop rest t (seen + 1)
          else loop rest t seen
  in
    loop tiles newtile 0

-- east = 1, south = 2, west = 3, north = 4
-- 1 to 4, 4 to 3, 3 to 2, 2 to 1
-- if self is east, if discard is from 4
-- otherwise if discard from (self - 1)

removeSublist : List a -> List a -> List a
removeSublist delete list =
  case list of
    [] ->
      []
    first::rest ->
      if List.any (\x -> x == first) delete then
        removeSublist (Tile.remove first delete) rest
      else first :: (removeSublist delete rest)

mapOnListList : List (List Int) -> Suit -> List (List Tile)
mapOnListList intMelds suit =
  case intMelds of
    [] ->
      []
    meld::rest ->
      let
        tiles = List.map (\n -> Tile (Tile.intToValue n) suit) meld
      in
        tiles :: (mapOnListList rest suit)

getMeldWith : List (List Tile) -> Tile -> List Tile
getMeldWith melds tile =
  case melds of
    [] ->
      []
    m::mRest ->
      if List.member tile m then
        m
      else getMeldWith mRest tile

-- returns (count, sequences, rest)
countSequences : List Tile -> (Int, List (List Tile), List Tile)
countSequences tiles =
  let
    original = List.head tiles
    numbers = List.map Tile.tileToInt tiles
    sortednumbers = List.sort numbers
    loop l number sequences leftover =
      case l of
        [] ->
          (number, sequences, List.reverse leftover)
        [first] ->
          (number, sequences, List.reverse (first::leftover))
        [first, second] ->
          (number, sequences, List.reverse (second::first::leftover))
        first::rest ->
          if List.member (first + 1) rest && List.member (first + 2) rest then
            loop
            (Tile.remove (first + 2) (Tile.remove (first + 1) rest))
            (number + 1)
            ([first, first + 1, first + 2]::sequences)
            leftover
          else
            loop
            rest
            number
            sequences
            (first::leftover)
  in
    case original of
      Nothing ->
        (0, [], [])
      Just tile ->
        let
          (ct, seqs, unused) = loop sortednumbers 0 [] []
        in
          ( ct
          , (mapOnListList seqs tile.suit)
          , (List.map (\n -> Tile (Tile.intToValue n) tile.suit) unused)
          )

-- tested and works
countPeng : List Tile -> (Int, List (List Tile), List Tile) -- (count, pengs, rest)
countPeng tiles =
  let
    loop l number pengs acc =
      case l of
        [] ->
          (number, pengs, acc) -- may need to reverse
        tile::rest ->
          let
            guess = countTiles rest tile + 1
            -- add 1 here to include current tile
          in
            if guess == 3 then
              let
                leftover = List.filter (\x -> x /= tile) l
              in
                loop
                leftover
                (number + 1)
                ([tile, tile, tile]::pengs)
                acc
            else
              loop rest number pengs (tile::acc)
  in
    loop tiles 0 [] []

-- tested
countGang : List Tile -> Tile -> (List Tile, List Tile) -- (gangs, rest)
countGang tiles newtile =
  if (countTiles tiles newtile) == 3 then
    ([newtile, newtile, newtile, newtile],
    -- should only have max 4 of same tile, so can use List.filter
    List.filter (\x -> x /= newtile) tiles)
  else
    ([], newtile::tiles)

handleNumbered : List Tile -> (Int, List Tile)
handleNumbered tiles =
  let
    -- calculate peng first and sequences first
    -- take greater result
    (seqNumber, sequences, leftover) = countSequences tiles --countPeng
    (pengNumber, pengs, leftover2) = countPeng leftover -- countSequences
    (pengNumber2, pengs2, leftover3) = countPeng tiles
    (seqNumber2, sequences2, leftover4) = countSequences leftover3
  in
    if seqNumber + pengNumber > pengNumber2 + seqNumber2 then
      (seqNumber + pengNumber, leftover2)
    else
      (seqNumber2 + pengNumber2, leftover4)

combineTuples : List (Int, List Tile) -> (Int, List Tile)
combineTuples tuples =
  List.foldl (\t1 t2 ->
    (Tuple.first t1 + Tuple.first t2, Tuple.second t1 ++ Tuple.second t2))
    (0, [])
    tuples

-- testing
checkWin : List Tile -> Int -> Bool
checkWin tiles declared =
  let
    numbered =
      [ handleNumbered (Tile.collect Dots tiles)
      , handleNumbered (Tile.collect Bamboo tiles)
      , handleNumbered (Tile.collect Characters tiles)
      ]
    (wNum, wPengs, wRest) = countPeng (Tile.collect Winds tiles)
    (dNum, dPengs, dRest) = countPeng (Tile.collect Dragons tiles)
    tuples = numbered ++ [(wNum, wRest)] ++ [(dNum, dRest)]
    (melds, leftover) = combineTuples tuples
  in
    case leftover of
      [eye1, eye2] ->
        eye1 == eye2 && melds + declared == 4
      _ ->
        False

checkForGang : List Tile -> Tile -> Bool -- only call with unflipped tiles
checkForGang tiles newtile =
  countTiles tiles newtile == 3

checkForPeng : List Tile -> Tile -> Bool -- only call with unflipped tiles
checkForPeng tiles newtile =
  countTiles tiles newtile == 2

checkForChi : List Tile -> Tile -> Bool
checkForChi tiles newtile =
  let
    (n, sequences, rest) = countSequences (newtile::tiles)
  in
    n > 0 && List.member newtile (List.concat sequences)

findDiscardHelper : List (Maybe Tile) -> Maybe Tile --orList
findDiscardHelper maybes =
  case maybes of
    [] ->
      Nothing
    first::rest ->
      case first of
        Nothing ->
          findDiscardHelper rest
        Just t ->
          Just t

-- tile -> (tile, list tile)
findDiscard : List Tile -> (Tile, List Tile)
findDiscard tiles =
  let
    discards =
      [ (findDiscardOrdinal (Tile.collect Winds tiles))
      , (findDiscardOrdinal (Tile.collect Dragons tiles))
      , (findDiscardInterval (Tile.collect Dots tiles))
      , (findDiscardInterval (Tile.collect Bamboo tiles))
      , (findDiscardInterval (Tile.collect Characters tiles)) ]
    first = findDiscardHelper discards
  in
    case first of
      Nothing ->
        case (List.head tiles) of
          Nothing ->
            Debug.todo "Exception: discard called on empty"
          Just ft ->
            (ft, Tile.remove ft tiles)
      Just t ->
        (t, Tile.remove t tiles)

findDiscardOrdinal : List Tile -> Maybe Tile
findDiscardOrdinal tiles =
  case tiles of
    [] ->
      Nothing
    t::rest ->
      if countTiles rest t == 0 then
        Just t
      else findDiscardOrdinal (List.filter (\x -> x /= t) rest)

findDiscardInterval : List Tile -> Maybe Tile
findDiscardInterval tiles =
  let
    sorted = Tile.sortNumbered tiles
  in
    case sorted of
      [] ->
        Nothing
      singleton::[] ->
        Just singleton
      first::second::rest ->
        -- if gap too big between two adjacent numbered tiles,
        -- i.e. chi not viable
        if (Tile.tileToInt second) - (Tile.tileToInt first) > 2 then
          Just first
        else
          findDiscardInterval rest

withDiscard : Tile -> List Tile -> Int -> Maybe Attempt
withDiscard tile tiles declared =
  let
    hand = tile::tiles
  in
    if checkWin hand declared then
      Just (Hu hand)
    else
      let
        (gangs, gangrest) = countGang tiles tile
      in
        case gangs of
          ganghd::gangtl ->
            Just (Gang (gangs, gangrest))
          [] ->
            let
              (pengCount, pengs, pengrest) = countPeng hand
              pengsWithTile = getMeldWith pengs tile
            in
              case pengsWithTile of
                penghd::pengtl ->
                  Just
                  (Peng (pengsWithTile, removeSublist pengsWithTile hand))
                [] ->
                  let
                    (seqCount, seqs, seqrest) =
                      (
                        case tile.suit of
                          Dots ->
                            countSequences
                              (Tile.collect Dots hand)
                          Bamboo ->
                            countSequences
                              (Tile.collect Bamboo hand)
                          Characters ->
                            countSequences
                              (Tile.collect Characters hand)
                          _ ->
                            (0, [], [])
                      )
                    seqsWithTile = getMeldWith seqs tile
                  in
                    case seqsWithTile of
                      seqhd::seqtl ->
                        Just
                        (Chi (seqsWithTile, removeSublist seqsWithTile hand))
                      [] ->
                        Nothing
  -- create a new list tile::tiles
  -- first check if that hand is winning
  -- then get gangs (requires countGang), pengs, sequences
  -- if a gang has the new tile in it, return attempt with that
  -- include peng and chi melds with their leftovers in the gang case
  -- when constructing leftover)
  -- if no gang, then try peng, then try chi, else Nothing.
  -- Debug.todo

testHand : List Tile -- win
testHand =
  [ Tile Three Bamboo
  , Tile Four Bamboo
  , Tile Five Bamboo
  , Tile White Dragons
  , Tile White Dragons
  , Tile White Dragons
  , Tile Seven Dots
  , Tile Eight Dots
  , Tile Nine Dots
  , Tile Red Dragons
  , Tile Red Dragons
  , Tile Red Dragons
  , Tile East Winds
  , Tile East Winds ]

testHand2 : List Tile -- win
testHand2 =
  [ Tile Five Dots
  , Tile Five Dots
  , Tile Seven Bamboo
  , Tile Seven Bamboo
  , Tile Seven Bamboo
  , Tile Four Characters
  , Tile Five Characters
  , Tile Six Characters
  , Tile Four Characters
  , Tile Five Characters
  , Tile Six Characters
  , Tile Six Characters
  , Tile Seven Characters
  , Tile Eight Characters ]

testHand3 : List Tile -- win
testHand3 =
  [ Tile Eight Dots
  , Tile Eight Dots
  , Tile Eight Dots
  , Tile Six Dots
  , Tile Six Dots
  , Tile Seven Bamboo
  , Tile Eight Bamboo
  , Tile Nine Bamboo
  , Tile South Winds
  , Tile South Winds
  , Tile South Winds
  , Tile Red Dragons
  , Tile Red Dragons
  , Tile Red Dragons ]

testHand4 : List Tile -- not win
testHand4 =
  [ Tile Three Bamboo
  , Tile Four Dots
  , Tile Five Dots
  , Tile Seven Dots
  , Tile Four Bamboo
  , Tile Six Bamboo
  , Tile Six Bamboo
  , Tile Red Dragons
  , Tile Red Dragons
  , Tile Green Dragons
  , Tile Green Dragons
  , Tile Green Dragons
  , Tile White Dragons
  , Tile White Dragons
  ]

testHand5 : List Tile -- win
testHand5 =
  [ Tile One Dots
  , Tile One Dots
  , Tile One Dots
  , Tile Three Dots
  , Tile Three Dots
  , Tile Three Bamboo
  , Tile Three Bamboo
  , Tile Three Bamboo
  , Tile Three Characters
  , Tile Three Characters
  , Tile Three Characters
  , Tile Four Characters
  , Tile Five Characters
  , Tile Six Characters
  ]
