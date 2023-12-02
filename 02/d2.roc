app "aoc23D01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
      pf.Stdout,
      "example.txt" as exampleInput : Str,
      "input.txt" as puzzleInput : Str,
    ]
    provides [main] to pf


Round : (Nat, Nat, Nat)
ParseError : [ErrorMsg Str]
Color : [Red, Green, Blue]
Game : (Nat, List Round)

parseGame : Str -> Result Game ParseError
parseGame = \ line ->
    Str.splitFirst line ":"
    |> Result.mapErr \_ -> ErrorMsg "\(line) is not a valid line"
    |> Result.try \{ before: header, after: revealsStr } -> 
      headerResult = parseGameID header
      reveals = parseRounds revealsStr

      when headerResult is
      Ok gameId -> Ok (gameId, reveals)
      Err _ -> Err (ErrorMsg "something failed")

expect
    res = parseGame "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

    res == Ok (5, [(6, 3, 1), (1, 2, 2)])

parseGameID = \game ->
    Str.splitFirst game " "
    |> Result.try \ { after } -> Str.toNat after
    |> Result.mapErr \ _ -> InvalidHeader

expect parseGameID "Game 23" == Ok 23
expect parseGameID "3240081" == Err InvalidHeader

parseRounds : Str -> (List Round)
parseRounds = \str ->
    Str.split str ";"
    |> List.map parseRound

parseRound : Str -> Round
parseRound = \ str ->
  str
  |> Str.split ","
  |> List.map Str.trim
  |> List.keepOks parseCubeColors
  |> List.walk (0, 0, 0) addRounds


parseCubeColors : Str -> Result Round ParseError
parseCubeColors = \line ->
  Str.splitFirst line " "
  |> Result.mapErr \_ -> ErrorMsg line
  |> Result.try \ {before: valueStr, after: colorStr} ->
    when Str.toNat valueStr is
    Ok value -> Ok (withColor (0, 0, 0) (parseColor colorStr) value)
    Err _ -> Err (ErrorMsg "could not parse \(valueStr) as number")

 
expect 
  pick = parseCubeColors "5 red"
  pick == Ok (5, 0, 0)

expect parseCubeColors "2 blue" == Ok (0, 0, 2)   


parseColor : Str -> Color
parseColor = \ str ->
  if str == "red" then
    Red
  else if str == "green" then
    Green
  else if str == "blue"  then
    Blue
  else
    crash "bad color"   

expect parseColor "red" == Red
expect parseColor "green" == Green
expect parseColor "blue" == Blue


withColor : Round, Color, Nat -> Round
withColor = \(r, g, b), color, value ->
    when color is
    Red -> (value, g, b)
    Green -> (r, value, b)
    Blue -> (r, g, value)


maxRound : Round, Round -> Round
maxRound = \(a, b, c), (x, y, z) ->
    (Num.max a x, Num.max b y, Num.max c z)

expect maxRound (1, 2, 4) (5, 1, 3) == (5, 2, 4)

addRounds : Round, Round -> Round
addRounds = \(a, b, c), (x, y, z) -> (a + x, b + y, z + c)

expect addRounds (1,2, 3) (6, 5, 4) == (7, 7, 7)



possibleRound : Round -> Bool
possibleRound = \(r, g, b) -> r <= 12 && g <= 13 && b <= 14

possibleGame : Game -> Bool
possibleGame = \(_, reveals) -> List.all reveals possibleRound

expect possibleGame (1, [(1, 2, 3)])
expect possibleGame (1, [(15, 2, 3)]) == Bool.false

computePower : Round -> Nat
computePower = \(r, g, b) -> r * g * b

expect computePower (1, 2, 3) == 100

getRounds : Game -> List Round
getRounds = \(_, reveals) -> reveals

main = 
  games = Str.split puzzleInput "\n"
    |> List.keepOks parseGame

  sumOfPossibleGames = games
    |> List.keepIf possibleGame
    |> List.map \(id, _) -> id
    |> List.sum

  sumOfPowers = games 
    |> List.map \g -> 
      g 
      |> getRounds 
      |> List.walk (0, 0, 0) maxRound
      |> computePower
    |> List.sum

  Stdout.line "part1: \(sumOfPossibleGames |> Num.toStr) \npart2: \(sumOfPowers |> Num.toStr)" 

  
