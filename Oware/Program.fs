module Oware

type StartingPosition =
    | South
    | North
type CurrentStatus = 
    | STurn  
    | NTurn
    | Draw 
    | NWin
    | SWin
type Player =
    | Player of house : int * int * int * int * int * int  * score : int
  type Board = 
      | Board of North : Player * South : Player  * Status : CurrentStatus
 

//let CurrentStatus = ["South's turn"; "North's turn"; "Game ended in a draw"; "South won"; "North won"]
//let SouthTurn:: _:: _:: _:: _ = CurrentStatus
//let _:: NorthTurn:: _:: _:: _ = CurrentStatus

let getSeeds n board = //failwith "Not implemented" 
    let (Board (_, SouthHouse, _)) = board
    let (Player(Shouse1, Shouse2, Shouse3, Shouse4, Shouse5, Shouse6, _)) = SouthHouse
    
    
    let (Board (NorthHouse, _, _)) = board
    let  (Player(Nhouse7, Nhouse8, Nhouse9, Nhouse10, Nhouse11, Nhouse12, _)) = NorthHouse
    
    match n with 
    |1 -> Shouse1 |2 -> Shouse2 |3 -> Shouse3 |4 -> Shouse4 |5 -> Shouse5 |6 -> Shouse6 |7 -> Nhouse7 |8 -> Nhouse8 |9 -> Nhouse9 |10 -> Nhouse10 |11 -> Nhouse11 |12 -> Nhouse12 
    |_ -> failwith "Not valid"
   
let useHouse n board = //failwith "Not implemented"
    //get status of board first, can only play with Sout turn or nor turn, if status is n turn, north needs to collect seeds from thier hous and play then change status to south
   
   let (Board (_, SouthHouse, _)) = board
   let (Player(Shouse1, Shouse2, Shouse3, Shouse4, Shouse5, Shouse6, SouthScore)) = SouthHouse
  
   let (Board (NorthHouse, _, _)) = board
   let (Player(Nhouse7, Nhouse8, Nhouse9, Nhouse10, Nhouse11, Nhouse12, NorthScore)) = NorthHouse

   let HouseList = [Shouse1; Shouse2; Shouse3; Shouse4; Shouse5; Shouse6; Nhouse7; Nhouse8; Nhouse9; Nhouse10; Nhouse11; Nhouse12]
   let HouseScore = (SouthScore, NorthScore)
   let setSeeds n houses = 
       let [a;b;c;d;e;f;g;h;i;j;k;l] = houses
       match n+1 with 
       | 1 -> [a+1;b;c;d;e;f;g;h;i;j;k;l]| 2 -> [a;b+1;c;d;e;f;g;h;i;j;k;l]| 3 -> [a;b;c+1;d;e;f;g;h;i;j;k;l]| 4 -> [a;b;c;d+1;e;f;g;h;i;j;k;l]| 5 -> [a;b;c;d;e+1;f;g;h;i;j;k;l]| 6 -> [a;b;c;d;e;f+1;g;h;i;j;k;l]
       | 7 -> [a;b;c;d;e;f;g+1;h;i;j;k;l]| 8 -> [a;b;c;d;e;f;g;h+1;i;j;k;l] | 9 -> [a;b;c;d;e;f;g;h;i+1;j;k;l]| 10 -> [a;b;c;d;e;f;g;h;i;j+1;k;l]| 11 -> [a;b;c;d;e;f;g;h;i;j;k+1;l] | 12 -> [a;b;c;d;e;f;g;h;i;j;k;l+1]

   let removeSeeds n seeds houses = 
    let [a;b;c;d;e;f;g;h;i;j;k;l] = houses
    match n with 
    | 1 -> [a-seeds;b;c;d;e;f;g;h;i;j;k;l] | 2 -> [a;b-seeds;c;d;e;f;g;h;i;j;k;l]| 3 -> [a;b;c-seeds;d;e;f;g;h;i;j;k;l] | 4 -> [a;b;c;d-seeds;e;f;g;h;i;j;k;l]| 5 -> [a;b;c;d;e-seeds;f;g;h;i;j;k;l] | 6 -> [a;b;c;d;e;f-seeds;g;h;i;j;k;l]
    | 7 -> [a;b;c;d;e;f;g-seeds;h;i;j;k;l]| 8 -> [a;b;c;d;e;f;g;h-seeds;i;j;k;l]| 9 -> [a;b;c;d;e;f;g;h;i-seeds;j;k;l]| 10 -> [a;b;c;d;e;f;g;h;i;j-seeds;k;l]| 11 -> [a;b;c;d;e;f;g;h;i;j;k-seeds;l] | 12 -> [a;b;c;d;e;f;g;h;i;j;k;l-seeds]
   
   let numSeeds n  houses = 
      let [a;b;c;d;e;f;g;h;i;j;k;l] = houses
      match n with 
      | 1 -> a | 2 -> b| 3 -> c | 4 -> d| 5 -> e | 6 -> f
      | 7 -> g| 8 -> h| 9 -> i| 10 -> j| 11 -> k | 12 -> l

   let distribute n seeds houses =  
       let rec move n seeds houses = 
           match seeds <> 0 with 
           | true -> 
                  match n > 11 && seeds > 0 with 
                  |true ->
                        let n = 0
                        move (n+1)  (seeds-1) (setSeeds n houses)                                 
                  |false -> move (n+1) (seeds-1) (setSeeds n houses)                                
           | false -> 
                     let lastHouse = n 
                     houses, lastHouse 
       move n seeds houses 
   
   let setScore n seeds xs  = 
               let rec checkSeeds lastHouse seeds xs northscore southscore = 
                match lastHouse = 0  with 
                | false ->
                       match lastHouse > 0 && lastHouse < 7 with 
                       | false -> xs, northscore, southscore
                       | true -> 
                            match seeds with 
                            | 3 | 2 -> 
                                   match lastHouse = 1 with 
                                   | true -> 
                                        let v  = removeSeeds lastHouse seeds xs
                                        checkSeeds (lastHouse) (numSeeds (lastHouse) (v)) (v) (northscore+seeds) (southscore)
                                   | false ->
                                        let v  = removeSeeds lastHouse seeds xs
                                        checkSeeds (lastHouse-1) (numSeeds (lastHouse-1) (v)) (v) (northscore+seeds) (southscore)
                            | _ -> xs, northscore, southscore
                | true -> xs, northscore, southscore
               checkSeeds n seeds xs 0 0   //fix to take in South Score 


   let makeIntoTuple (xs,n) = 
       let [a;b;c;d;e;f],g = xs,n
       match xs,n with 
       | [_;_;_;_;_;_],_ -> (a, b, c, d,e,f, g)
       | [y;z],_ ->  failwith "never matched"

   let (Board (_, _, BoardStatus)) = board
   match BoardStatus with 
   | NTurn -> 
        match n >= 7 && n <= 12 with 
        | true ->
             let  NumOfSeeds = getSeeds n board
             let houses = removeSeeds n NumOfSeeds HouseList
             let HousesAfterSowing = distribute n (NumOfSeeds) (houses)
             let HousesBeforeCapture,houseOfLastMove = HousesAfterSowing
             
             let HousesAfterCapture = setScore (houseOfLastMove) (numSeeds houseOfLastMove HousesBeforeCapture) (HousesBeforeCapture)
            
             let hey,hi = HouseScore
              
             let lst,score1,score2 = HousesAfterCapture
             let newNorthHouses = List.skip(6)lst
             let newSouthHouses = List.take(6)lst
             let nTup = makeIntoTuple (newNorthHouses,score1)
             let sTup = makeIntoTuple (newSouthHouses, score2)
             
             let (seven, eight, nine, ten, eleven, twelve, hi) = nTup

            
             let (one, two, three, four, five, six, hey) = sTup
             
             Board(Player(seven, eight, nine, ten, eleven, twelve, hi),  Player(one, two, three, four, five, six, hey), STurn)
             
             //setScore 3 3 [5;2;3;4;4;4;4;4;4;4;4;4]
            
        | false -> board
  
   | STurn -> 
        match n >= 1 && n <= 6 with 
        | true -> 

               let  NumOfSeeds = getSeeds n board
               let houses = removeSeeds n NumOfSeeds HouseList
               
               
               //setScore 3 3 [5;2;3;4;4;4;4;4;4;4;4;4]
               let distribute n seeds houses =  
                   let rec move n seeds houses = 
                       match seeds <> 0 with 
                       | true -> 
                              match n > 11 && seeds > 0 with 
                              |true ->
                                    let n = 0
                                    move (n+1)  (seeds-1) (setSeeds n houses)                                 
                              |false -> move (n+1) (seeds-1) (setSeeds n houses)                                
                       | false -> 
                                 let lastHouse = n 
                                 houses, lastHouse 
                   move n seeds houses 
               
               let setScore n seeds xs  = 
                           let rec checkSeeds lastHouse seeds xs northscore southscore = 
                            match lastHouse = 0  with 
                            | false ->
                                   match lastHouse > 6 && lastHouse < 13 with 
                                   | false -> xs, northscore, southscore
                                   | true -> 
                                        match seeds with 
                                        | 3 | 2 -> 
                                               match lastHouse = 1 with 
                                               | true -> 
                                                    let v  = removeSeeds lastHouse seeds xs
                                                    checkSeeds (lastHouse) (numSeeds (lastHouse) (v)) (v) (northscore+seeds) (southscore)
                                               | false ->
                                                    let v  = removeSeeds lastHouse seeds xs
                                                    checkSeeds (lastHouse-1) (numSeeds (lastHouse-1) (v)) (v) (northscore+seeds) (southscore)
                                        | _ -> xs, northscore, southscore
                            | true -> xs, northscore, southscore
                           checkSeeds n seeds xs 0 0   //fix to take in South Score 
               let HousesAfterSowing = distribute n (NumOfSeeds) (houses)
               let HousesBeforeCapture,houseOfLastMove = HousesAfterSowing
               
               let HousesAfterCapture = setScore (houseOfLastMove) (numSeeds houseOfLastMove HousesBeforeCapture) (HousesBeforeCapture)
            
               
                
               let lst,score1,score2 = HousesAfterCapture
               let newNorthHouses = List.skip(6)lst
               let newSouthHouses = List.take(6)lst
               let nTup = makeIntoTuple (newNorthHouses,score1)
               let sTup = makeIntoTuple (newSouthHouses, score2)
               let hey,hi = HouseScore
               let (seven, eight, nine, ten, eleven, twelve, hi) = nTup

            
               let (one, two, three, four, five, six, hey) = sTup
               
               Board(Player(seven, eight, nine, ten, eleven, twelve, hi),  Player(one, two, three, four, five, six, hey), NTurn)
              //let NumOfSeeds = getSeeds n board
              //let changedHouses = distribute n (NumOfSeeds) (HouseList)
              
        | false -> board

   | Draw -> failwith "Perhaps return the two boards???"
   | SWin -> failwith "Return south board, they won ???"
   | NWin -> failwith "Return north board, they won ???"



let start position = //failwith "Not implemented"
    let NorthPlayer = Player (4, 4, 4, 4, 4, 4, 0)
    let SouthPlayer = Player (4, 4, 4, 4, 4, 4, 0)

    
    match position with
    | North -> Board (NorthPlayer, SouthPlayer, NTurn)
    | South -> Board (NorthPlayer, SouthPlayer, STurn)
  
let score board = //failwith "Not implemented"
  //let NumOfSeeds = getSeeds n board
  let (Board (_, SouthVal, _)) = board
  let (Board (NorthVal, _, _)) = board
  let (Player(_, _, _, _, _, _, Sscore)) = SouthVal
  let (Player(_, _, _, _, _, _, Nscore)) = NorthVal
  let NorthScore = Nscore
  let SouthScore = Sscore
  match board with 
  | (Board (_, _, _)) -> NorthScore, SouthScore
  | _ -> failwith "You will never be matched"

let gameState board = //failwith "Not implemented"
  let(Board (_, _, status)) = board
  match status with 
  | NTurn -> "North's turn"
  |  STurn -> "South's turn"
  | Draw -> "Game ended in a draw"
  | SWin -> "South Won"
  | NWin -> "North Won"
   
[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
