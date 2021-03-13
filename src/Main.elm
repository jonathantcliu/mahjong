module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Browser
import Array
import Random
import Random.List exposing (shuffle)
import Tile exposing (Tile, Suit(..), Value(..))
import Strategy exposing (Attempt(..))

type alias Model =
  { deck : List Tile
  , playerHand : List Tile
  , cpu1Hand : List Tile
  , cpu2Hand : List Tile
  , cpu3Hand : List Tile
  , playerShown : List Tile
  , cpu1Shown : List Tile
  , cpu2Shown : List Tile
  , cpu3Shown : List Tile
  , playerMelds : Int
  , cpu1Melds : Int
  , cpu2Melds : Int
  , cpu3Melds : Int
  , playerSelected : Maybe Tile
  , east : Int
  , south : Int
  , west : Int
  , north : Int
  , turn : Int
  , discard : Maybe DiscardedTile
  , request : Maybe Request
  , message : String
  , canNewGame : Bool
  , canHu : Bool
  , canGang : Bool
  , canPeng : Bool
  , canChi : Bool
  , justMelded : Bool
  , gangTiles : Maybe (List Tile, List Tile)
  , pengTiles : Maybe (List Tile, List Tile)
  , chiTiles : Maybe (List Tile, List Tile) }
  -- (List Tile, List Tile) is (meld, leftover tiles)

type Direction
  = East  -- 0, can take is (discarder == (self + 3) % 4)
  | South -- 1
  | West  -- 2
  | North -- 3

type alias Request =
  {attempt : Attempt, requester : Int}

type alias DiscardedTile =
  {tile : Tile, discarder : Int}

init : () -> ( Model, Cmd Msg )
init _ =
    ( { deck = []
      , playerHand = []
      , cpu1Hand = []
      , cpu2Hand = []
      , cpu3Hand = []
      , playerShown = []
      , cpu1Shown = []
      , cpu2Shown = []
      , cpu3Shown = []
      , playerMelds = 0
      , cpu1Melds = 0
      , cpu2Melds = 0
      , cpu3Melds = 0
      , playerSelected = Nothing
      , east = 0
      , south = 1
      , west = 2
      , north = 3
      , turn = 0
      , discard = Nothing
      , request = Nothing
      , message = "Welcome!"
      , canNewGame = True
      , canHu = False
      , canGang = False
      , canPeng = False
      , canChi = False
      , justMelded = False
      , gangTiles = Nothing
      , pengTiles = Nothing
      , chiTiles = Nothing }
    , Cmd.none )

type Msg
  = NewGame (Int)
  | ShuffleDeck (List Tile)
  | PlayerSelect (Int)
  | PlayerDiscard --playerSelect should be non-Nothing
  | PlayerHu -- discard should be non-Nothing
  | PlayerGang -- discard should be non-Nothing
  | PlayerPeng -- discard should be non-Nothing
  | PlayerChi -- discard should be non-Nothing
  | RunGame
  | CheckRequests -- every x seconds

shuffleDeck : Cmd Msg
shuffleDeck =
  Random.generate ShuffleDeck (shuffle Tile.initDeck)
  --Random.generate ShuffleDeck
  --(Random.Array.shuffle (Array.fromList Tile.initDeck))
  {-
  Tile.initDeck
    |> Array.fromList
    |> Random.Array.shuffle
    |> Random.generate ShuffleDeck -}

view : Model -> Html Msg
view model =
  div []
    [ div [ attribute "class" "play-table-column" ]
      [ h1 [] [ text "🀄 Mahjong 🀄" ]
      , div [ attribute "class" "message" ]
        [ text (model.message ++ " " ++ Debug.toString (model.gangTiles) ++ " " ++ Debug.toString (model.pengTiles) ++ " " ++ Debug.toString (model.chiTiles)) ]
      , div [ attribute "class" "new-game" ]
        (if model.canNewGame then
          [ div [] [ button [ onClick (NewGame 0) ] [ text "Start as East" ] ]
          ]
        else
          [ div []
            [ button [ onClick (NewGame 0), attribute "disabled" "true" ]
                     [ text "Game in Progress" ] ] ]
        )
      , table [ attribute "class" "show-computer" ]
        [ thead []
          [ tr []
            [ th [] [ text "CPU 1" ]
            , th [] [ text "CPU 2" ]
            , th [] [ text "CPU 3" ]
            ]
          ]
        , tbody []
          [ tr []
            [ td []
              (makeSpans (Tile.showPlayerHand model.cpu1Shown) 30)
            , td []
              (makeSpans (Tile.showPlayerHand model.cpu2Shown) 30)
            , td []
              (makeSpans (Tile.showPlayerHand model.cpu3Shown) 30)
            ]
          ]
          , tr []
            [ td []
              (makeSpans (Tile.showPlayerHand (Tile.sortHand model.cpu1Hand)) 30) -- showCPUHand
            , td []
              (makeSpans (Tile.showPlayerHand (Tile.sortHand model.cpu2Hand)) 30)
            , td []
              (makeSpans (Tile.showPlayerHand (Tile.sortHand model.cpu3Hand)) 30)
          ]
        ]
      ]
    , table [ attribute "class" "discardtile" ]
      [ thead []
        [ tr []
          [ th [] (makeDiscardSpan model.discard 80)
          ]
        ]
      ]
    , table [ attribute "class" "show-player" ]
      [ thead []
        [ tr []
          [ th [] [ text "Player" ]
          ]
        ]
      , tbody []
        [ tr []
          [ td []
            (makeSpans (Tile.showPlayerHand model.playerShown) 50)
          ]
        ]
        , tr []
          [ td []
            (makePlayerSpans
              (Tile.showPlayerHand (Tile.sortHand model.playerHand)) 80)
        ]
      ]
    , table [ attribute "class" "interaction-buttons" ]
      [ thead []
        [ tr []
          [ th []
            --[ button [ onClick (NewGame 0) ] [ text "TEST 1" ] ]
            [ text "" ]
          ]
        ]
        , tbody []
          [ tr []
            [ td []
              [
                (if model.turn == 0 &&
                    model.discard == Nothing &&
                    not model.canNewGame then
                  button [ onClick PlayerDiscard ] [ text "出牌" ]
                else
                  button
                    [ onClick PlayerDiscard, attribute "disabled" "true" ]
                    [ text "无法出牌" ]
                )
              ]
            , td []
              --[ button [ onClick PlayerHu ] [ text "和" ] ]
              [
                (if model.canHu then
                  button [ onClick PlayerHu ] [ text "和" ]
                else
                  button
                    [ onClick PlayerHu, attribute "disabled" "true" ]
                    [ text "无法和牌" ]
                )
              ]
            , td []
              --[ button [ onClick (PlayerGang) ] [ text "杠" ] ]
              [
                (if model.canGang then
                  button [ onClick PlayerGang ] [ text "杠" ]
                else
                  button
                    [ onClick PlayerGang, attribute "disabled" "true" ]
                    [ text "无法杠牌" ]
                )
              ]
            , td []
              --[ button [ onClick (PlayerPeng) ] [ text "碰" ] ]
              [
                (if model.canPeng then
                  button [ onClick PlayerPeng ] [ text "碰" ]
                else
                  button
                    [ onClick PlayerPeng, attribute "disabled" "true" ]
                    [ text "无法碰牌" ]
                )
              ]
            , td []
              --[ button [ onClick (PlayerChi) ] [ text "吃" ] ]
              [
                (if model.canChi then
                  button [ onClick PlayerChi ] [ text "吃" ]
                else
                  button
                    [ onClick PlayerChi, attribute "disabled" "true" ]
                    [ text "无法吃牌" ]
                )
              ]
          ]
        ]
      ]
    ]

makeSpans : List String -> Int -> List (Html msg)
makeSpans hand n =
  let
    len = List.length hand
    s = Debug.toString n -- (n + (50 - (len * 17)))
    spanmaker x =
      span [ attribute "style" ("font-size: " ++ s ++ "px;")] [text x]
    in
      List.map spanmaker hand

makeDiscardSpan : Maybe DiscardedTile -> Int -> List (Html Msg)
makeDiscardSpan mdt n =
  case mdt of
    Nothing ->
      []
    Just dt ->
      makeSpans (Tile.showPlayerHand [dt.tile]) n

makePlayerSpans : List String -> Int -> List (Html Msg)
makePlayerSpans hand n =
  let
    len = List.length hand
    s = Debug.toString n --(n + (50 - (len * 17)))
    spanmaker index x =
      span
        [ attribute "style" ("font-size: " ++ s ++ "px;")]
        [ button
          [ style "font-size" (s ++ "px")
          , style "background" "none"
          , style "border" "none"
          , style "margin" "0"
          , style "padding" "0"
          , style "cursor" "pointer"
          , onClick (PlayerSelect index) ]
          [ text x ]
        ]
    in
      List.indexedMap spanmaker hand

{-
remove : a -> List a -> List a
remove x xs =
  case xs of
    [] ->
      []
    first::rest ->
      if first == x then rest
      else first :: remove x rest -}

assign : Model -> Int -> Model
assign model n =
  if n == 0 then
    { model | east = 0, south = 1, west = 2, north = 3, turn = 0 }
  else if n == 1 then
    { model | east = 1, south = 2, west = 3, north = 0, turn = 1 }
  else if n == 2 then
    { model | east = 2, south = 3, west = 0, north = 1, turn = 2 }
  else if n == 3 then
    { model | east = 3, south = 0, west = 1, north = 2, turn = 3 }
  else model

getHand : Model -> Int -> List Tile
getHand model n =
  if n == 0 then model.playerHand
  else if n == 1 then model.cpu1Hand
  else if n == 2 then model.cpu2Hand
  else if n == 3 then model.cpu3Hand
  else []

getMelds : Model -> Int -> Int
getMelds model n =
  if n == 0 then model.playerMelds
  else if n == 1 then model.cpu1Melds
  else if n == 2 then model.cpu2Melds
  else if n == 3 then model.cpu3Melds
  else 0

getShown : Model -> Int -> List Tile
getShown model n =
  if n == 0 then model.playerShown
  else if n == 1 then model.cpu1Shown
  else if n == 2 then model.cpu2Shown
  else if n == 3 then model.cpu3Shown
  else []

updateHand : Model -> List Tile -> Int -> Model
updateHand model tiles n =
  if n == 0 then { model | playerHand = tiles }
  else if n == 1 then { model | cpu1Hand = tiles }
  else if n == 2 then { model | cpu2Hand = tiles }
  else if n == 3 then { model | cpu3Hand = tiles }
  else model

addShown : Model -> List Tile -> Int -> Model
addShown model tiles n =
  if n == 0 then { model | playerShown = model.playerShown ++ tiles
                 , playerMelds = model.playerMelds + 1 }
  else if n == 1 then { model | cpu1Shown = model.cpu1Shown ++ tiles
                      , cpu1Melds = model.cpu1Melds + 1 }
  else if n == 2 then { model | cpu2Shown = model.cpu2Shown ++ tiles
                      , cpu2Melds = model.cpu2Melds + 1 }
  else if n == 3 then { model | cpu3Shown = model.cpu3Shown ++ tiles
                      , cpu3Melds = model.cpu3Melds + 1 }
  else model

playerSelect : Model -> Int -> Model
playerSelect model n =
  let
    select = List.head (List.drop n model.playerHand)
  in
    case select of
      Nothing ->
        model
      Just t ->
        { model
          | playerSelected = Just t }
          --, message = "selected " ++ Debug.toString t }

getMove : Model -> Int -> Tile -> Int -> (Model, Cmd Msg)
getMove m cpu t discarder =
    if cpu > 3 then (m, Cmd.none)
    else if cpu == discarder then getMove m (cpu + 1) t discarder
    else
      let
        attempt = Strategy.withDiscard t (getHand m cpu) (getMelds m cpu)
      in
        case attempt of
          Nothing ->
            getMove m (cpu + 1) t discarder
          Just a ->
            case a of
              Hu _ -> -- probably can use tiles passed here haha
                let
                  cpuHand = getHand m cpu
                  newHand =
                    case m.discard of
                      Nothing ->
                        cpuHand
                      Just dt ->
                        dt.tile::cpuHand
                in -- may not need CheckRequests here
                  -- update
                  -- CheckRequests
                  ((updateHand
                  { m
                  | message = "game over, CPU " ++
                      Debug.toString cpu ++
                      " wins!"
                  , discard = Nothing
                  , request = Nothing
                  , canNewGame = True }
                  newHand
                  cpu), Cmd.none)
                  {-
                  ( updateHand
                    { m
                    | message = "game over, CPU " ++
                        Debug.toString cpu ++
                        " wins!"
                    , discard = Nothing
                    , canNewGame = True }
                    newHand
                    cpu, Cmd.none ) -}
              Gang (gang, rest) -> -- overrules anything except for Hu
                -- let
                -- newModel = updateHand m rest cpu
                -- in
                getMove
                { m | request = Just (Request (Gang (gang, rest)) cpu) }
                (cpu + 1)
                t
                discarder
              Peng (peng, rest) ->
                case m.request of
                  Just g ->
                    case g.attempt of
                      Gang (_, _) ->
                        getMove
                        { m | message = "overruled"}
                        (cpu + 1)
                        t
                        discarder
                      _ ->
                        getMove
                        { m
                          | request = Just (Request (Peng (peng, rest)) cpu) }
                        (cpu + 1)
                        t
                        discarder
                  Nothing ->
                    getMove
                    { m
                      | request = Just (Request (Peng (peng, rest)) cpu) }
                    (cpu + 1)
                    t
                    discarder
              Chi (chi, rest) ->
                case m.request of
                  Nothing ->
                    if discarder == modBy 4 (cpu + 3) then
                      getMove
                      { m
                        | request = Just (Request (Chi (chi, rest)) cpu) }
                      (cpu + 1)
                      t
                      discarder
                    else
                      getMove
                      { m | message = "chi not allowed" }
                      (cpu + 1)
                      t
                      discarder
                  _ ->
                    getMove
                    { m | message = "overruled"}
                    (cpu + 1)
                    t
                    discarder

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewGame n ->
      let
        assigned = assign (Tuple.first (init ())) n
      in
        ( { assigned | canNewGame = False }, shuffleDeck )
    ShuffleDeck tiles ->
      let
        hands = Tile.dealAll tiles
      in
        update RunGame
          { model
          | deck = hands.deck
          , playerHand = Tile.sortHand hands.playerHand
          , cpu1Hand = Tile.sortHand hands.cpu1Hand
          , cpu2Hand = Tile.sortHand hands.cpu2Hand
          , cpu3Hand = Tile.sortHand hands.cpu3Hand }
    RunGame ->
      if model.deck == [] then
        ( { model
          | message = "draw!"
          , canNewGame = True
          , turn = 0
          , canHu = False
          , canGang = False
          , canPeng = False
          , canChi = False },
        Cmd.none )
      else
        case model.discard of
          Nothing ->
            if model.turn == 0 then -- combine these if/then/else statements
              if model.justMelded then -- justMelded for CPU too!
                ({model | justMelded = False}, Cmd.none)
              else
                let
                  (newHand, newDeck)
                    = Tile.deal (getHand model model.turn) model.deck 1
                  hu = Strategy.checkWin newHand model.playerMelds
                  updatedModel =
                    updateHand model (Tile.sortHand newHand) model.turn
                in
                  ( { updatedModel
                      | deck = newDeck
                      , canHu = hu
                      , canGang = False --暗杠? calculate gangTiles earlier
                      , canPeng = False
                      , canChi = False },
                  Cmd.none )
            else
              if model.justMelded then
                let
                  (toDiscard, leftover) =
                    Strategy.findDiscard (getHand model model.turn)
                  updatedModel = updateHand model leftover model.turn
                in
                  -- update
                  -- RunGame
                  ( { updatedModel
                    -- | deck = newDeck
                      | discard = Just (DiscardedTile toDiscard model.turn)
                      , justMelded = False
                      , canGang = False --暗杠? not player though, add request
                      , canPeng = False
                      , canChi = False }, Cmd.none )
              else
                let
                  (newHand, newDeck)
                    = Tile.deal (getHand model model.turn) model.deck 1
                  (toDiscard, leftover) = Strategy.findDiscard newHand
                  updatedModel = updateHand model leftover model.turn
                in
                  update
                  RunGame
                  { updatedModel
                  | deck = newDeck
                  , discard = Just (DiscardedTile toDiscard model.turn)
                  , canGang = False --暗杠?
                  , canPeng = False
                  , canChi = False }
          Just dt ->
            let
              (t, discarder) = (dt.tile, dt.discarder)
            in
              if model.canNewGame then
                (model, Cmd.none)
              else if discarder == 0 then
                getMove model 1 t discarder
              else
                let
                  hand = t::model.playerHand
                  hu = Strategy.checkWin hand model.playerMelds
                  (gangs, gangrest) = Strategy.countGang model.playerHand t
                  (pengCount, pengs, pengrest) =
                    Strategy.countPeng hand
                  pengsWithTile = Strategy.getMeldWith pengs t
                  (seqCount, seqs, seqrest) =
                    case t.suit of
                      Dots ->
                        Strategy.countSequences (Tile.collect Dots hand)
                      Bamboo ->
                        Strategy.countSequences (Tile.collect Bamboo hand)
                      Characters ->
                        Strategy.countSequences (Tile.collect Characters hand)
                      _ ->
                        (0, [], [])
                  seqsWithTile = Strategy.getMeldWith seqs t
                  gangT =
                    if gangs == [] then Nothing else Just (gangs, gangrest)
                  pengT =
                    case pengsWithTile of
                      [] ->
                        Nothing
                      l ->
                        Just (l, Strategy.removeSublist l hand)
                  chiT =
                    case seqsWithTile of
                      [] ->
                        Nothing
                      l ->
                        Just (l, Strategy.removeSublist l hand)
                in
                  getMove
                  { model
                  | canHu = hu
                  , canGang = Strategy.checkForGang model.playerHand t -- gangs /= []
                  , canPeng = not hu &&
                      Strategy.checkForPeng model.playerHand t -- pengCount > 0
                      -- if hu, disable peng
                  , canChi = not hu &&
                      seqsWithTile /= [] && discarder == 3 -- seqCount > 0
                      -- if hu, disable chi
                  , gangTiles = gangT
                  , pengTiles = pengT
                  , chiTiles = chiT
                  , message = "calculated tiles" }
                  1
                  t
                  discarder
    CheckRequests ->
      case model.request of
        Nothing ->
          if model.canNewGame ||
          (model.turn == 0 && model.discard == Nothing) then
            (model, Cmd.none)
          else
            update
            RunGame
            { model
            | turn = modBy 4 (model.turn + 1)
            , discard = Nothing
            , gangTiles = Nothing
            , pengTiles = Nothing
            , chiTiles = Nothing }
        Just r ->
          let
            shownModel meld = addShown model meld r.requester
            updatedModel m =
              { m
              | turn = r.requester
              , canGang = False
              , canPeng = False
              , canChi = False
              , request = Nothing
              , discard = Nothing
              , gangTiles = Nothing
              , pengTiles = Nothing
              , chiTiles = Nothing }
          in
            case r.attempt of
              Hu tiles ->
                ({model | message = "winner: " ++ Debug.toString r.requester},
                Cmd.none)
              Gang (gang, rest) ->
                let
                  newModel =
                    updatedModel
                      (updateHand (shownModel gang) rest r.requester)
                in
                  update
                  RunGame
                  { newModel
                  | justMelded = False }
              Peng (peng, rest) ->
                let
                  newModel =
                    updatedModel
                      (updateHand (shownModel peng) rest r.requester)
                in
                  update
                  RunGame
                  { newModel
                  | justMelded = True } -- was r.requester == 0
              Chi (chi, rest) ->
                let
                  newModel =
                    updatedModel
                      (updateHand (shownModel chi) rest r.requester)
                in
                  update
                  RunGame
                  { newModel
                  | justMelded = True } -- was r.requester == 0
    PlayerSelect n ->
      ( playerSelect model n
      , Cmd.none )
    PlayerDiscard -> -- call RunGame 2021/03/07
      case model.playerSelected of
        Nothing ->
          (model, Cmd.none)
        Just t ->
          let
            newHand = Tile.remove t model.playerHand
          in
            update
            RunGame
            { model
            | playerSelected = Nothing
            , discard = Just (DiscardedTile t 0)
            , playerHand = newHand
            , turn = modBy 4 (model.turn)
            , justMelded = False }
    PlayerHu ->
      ( { model
        | message =
            case model.discard of
              Nothing ->
                "you self-touch win, you god!"
              Just dt ->
                "you win off of CPU " ++
                  Debug.toString dt.discarder ++
                  "'s discard!"
        , canNewGame = True
        , playerHand =
            case model.discard of
              Nothing ->
                model.playerHand
              Just dt ->
                dt.tile::model.playerHand
        , discard = Nothing
        , turn = 0
        , canHu = False
        , canGang = False
        , canPeng = False
        , canChi = False },
      Cmd.none )
    PlayerGang ->
      case model.gangTiles of
        Nothing ->
          update
          CheckRequests
          {model
          | playerHand = []
          , message = model.message ++ " big bad" }
        Just (g, r) ->
          update
          CheckRequests
          { model | request = Just (Request (Gang (g, r)) 0) }
    PlayerPeng ->
      case model.pengTiles of
        Nothing ->
          update
          CheckRequests
          { model
          | playerHand = []
          , message = model.message ++ " big bad" }
        Just (p, r) ->
          case model.request of
            Nothing ->
              update
              CheckRequests
              { model | request = Just (Request (Peng (p, r)) 0) }
            Just req ->
              case req.attempt of
                Gang _ ->
                  update
                  CheckRequests
                  model
                  --{ model | message = "overruled" }
                _ ->
                  update
                  CheckRequests
                  { model | request = Just (Request (Peng (p, r)) 0) }
    PlayerChi ->
      case model.chiTiles of
        Nothing ->
          update
          CheckRequests
          { model
          | playerHand = []
          , message = model.message ++ " big bad" }
        Just (c, r) ->
          case model.request of
            Nothing ->
              update
              CheckRequests
              { model | request = Just (Request (Chi (c, r)) 0) }
            _ ->
              update
              CheckRequests
              model

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 2000 (Basics.always CheckRequests)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
