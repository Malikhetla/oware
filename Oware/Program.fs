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
   let HouseScore = (NorthScore, SouthScore)
   let setSeeds n houses = 
       let [a;b;c;d;e;f;g;h;i;j;k;l] = houses
       match n with 
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
   
   (*
     distribute sows to each house one seed at a time, 
     it returns a list of house where seeds have been added and house where last seed was sown
   *)
   let distribute num seeds houses =  
       let rec move n seeds houses = 
           match seeds <> 0 with //keep sowing when there are still more seeds
           | true -> 
                 match n = num with 
                 | true -> move (n+1) (seeds) (houses)
                 | false -> 
                      match n > 12 && seeds > 0 with // when there are still more seeds to be sown, wrap around to the beginning 
                      |true ->
                            let n = 1
                            move (n+1)  (seeds-1) (setSeeds (n) houses) 
                      |false -> 
                              move (n+1) (seeds-1) (setSeeds n houses)
           | false -> 
                     let lastHouse = (n-1)
                     houses, lastHouse //returns houses with seeds added and house where last seed was sown
       move num seeds houses 

   (*
    checkHouses looks through all 6 houses on opponent's side 
    if there is only one house with 2 or 3 seeds which the current player wants to capture 
    such seeds remain as opponent will have no seeds to play with 
   *)
   let checkHouses xs = 
       let rec seedsMoreThanThree count xs = 
            match xs with 
            | [] -> 
                 count
            | a:: rest -> seedsMoreThanThree (count+a) (rest)
       seedsMoreThanThree 0 xs

   let setScoreNorth n seeds xs  = 
               let rec checkSeeds lastHouse seeds xs northscore southscore = 
                match lastHouse = 0  with 
                | false ->
                       match lastHouse > 0 && lastHouse < 7 with //north player captures on south side (houses from 1 to 6)
                       | false -> xs, northscore, southscore // if house where last seed was sown is not on south side, do not capture 
                       | true -> 
                            match seeds with 
                            | 2 | 3 -> //check if house has two or three seeds (that can potentially be captured) 
                                let avail = checkHouses (List.take(6)xs) //checks that 2 or 3 seeds remaining from south side are not captured 
                                match avail <= 3 && (numSeeds lastHouse xs = 3 || numSeeds lastHouse xs = 2) with 
                                | true -> xs, northscore, southscore   // return list, otherwise opponent's board will be empty
                                | false -> 
                                        match lastHouse with 
                                        | 1 -> 
                                            let p = removeSeeds lastHouse seeds xs
                                            p, northscore+seeds, southscore
                                        | _ -> 
                                            let v  = removeSeeds lastHouse seeds xs
                                            checkSeeds (lastHouse-1) (numSeeds (lastHouse-1) (v)) (v) (northscore+seeds) (southscore)
                            | _ -> xs, northscore, southscore
                | true -> xs, northscore, southscore
               checkSeeds n seeds xs 0 0   
   
   let setScoreSouth n seeds xs  = 
       let rec checkSeeds lastHouse seeds xs northscore southscore = 
        match lastHouse <= 6  with 
        | false ->
               match lastHouse > 6 && lastHouse < 13 with //north player captures on south side (houses from 1 to 6)
               | false -> xs, northscore, southscore // if house where last seed was sown is not on south side, do not capture 
               | true -> 
                    match seeds with 
                    | 2 | 3 -> //check if house has two or three seeds (that can potentially be captured) 
                           let avail = checkHouses (List.skip(6)xs) //checks that 2 or 3 seeds remaining from south side are not captured 
                           match avail <= 3 && (numSeeds lastHouse xs = 3 || numSeeds lastHouse xs = 2)  with 
                           | true -> xs, northscore, southscore   // return list, otherwise opponent's board will be empty
                           | false -> 
                                   
                                let v  = removeSeeds lastHouse seeds xs
                                checkSeeds (lastHouse-1) (numSeeds (lastHouse-1) (v)) (v) (northscore) (southscore+seeds)
                    | _ -> xs, northscore, southscore
        | true -> xs, northscore, southscore
       checkSeeds n seeds xs 0 0   

   let makeIntoTuple (xs,n) = 
       let [a;b;c;d;e;f],g = xs,n
       match xs,n with 
       | [_;_;_;_;_;_],_ -> (a, b, c, d,e,f, g)
       | [y;z],_ ->  failwith "never matched"
  // let checkSeedNumber n = 
        
                 
      
   let (Board (_, _, BoardStatus)) = board
   match BoardStatus with 
    // 1. FIND OUT WHICH PLAYER'S TURN IT IS. 
   | NTurn -> 
        let housesToPlay = checkHouses(List.skip(6)HouseList)
        match housesToPlay with 
        | 0 -> (Board (NorthHouse, SouthHouse, STurn))
        | _ -> 
            match n >= 7 && n <= 12 with // north player can only pick seeds between houses 7 and 12 
            | true -> //find a better wayt to rewrite this !!!!!!!

                 (* 
                    findSeeds ensures that north player does not try to distribute 0 seeds so 
                    it attempts to the next house (within houses 7 to 12) with seeds.
                 *)
                 let findSeeds house seeds =  
                   let rec search house seeds = 
                      match seeds = 0 with 
                      | true -> 
                            match house = 12 with 
                            | true -> 
                                   let house = 7 
                                   search house (getSeeds house board)
                            | false -> search (house+1) (getSeeds (house+1) (board))
                      | false -> seeds 
                   search house seeds 
                 // 2. IF IT'S NORTH'S TURN, GET SEEDS FROM ALLOCATED HOUSE BUT MAKE SURE HOUSE HAS SEEDS, see 'findSeeds'
                 let tryThis = checkHouses (List.skip(6)HouseList)
                 match tryThis > 0 && getSeeds n board = 0 with 
                 | true -> Board(NorthHouse, SouthHouse, NTurn)
                 | _ -> 
                     let numOfSeeds = findSeeds n (getSeeds n board) 
                     // 3. REMOVE SEEDS FROM THIS HOUSE AS THEY ARE TO BE GIVEN TO OTHER HOUSES 
                     let houses = removeSeeds n numOfSeeds HouseList
                     // 4. SOW ONE SEED TO EACH SUCCESSIVE HOUSE IN COUNTERCLOCKWISE MANNER
                     let HousesAfterSowing = distribute n (numOfSeeds) (houses)
             
                     let HousesBeforeCapture,houseOfLastMove = HousesAfterSowing //returns list with seeds added and house where last seed was sown 
                     match houseOfLastMove < 7 && ((numSeeds houseOfLastMove HousesBeforeCapture) = 2 || (numSeeds houseOfLastMove HousesBeforeCapture) = 3) with 
                     | false ->
                            //let lst,score1,score2 = HousesAfterCapture
                            let newNorthHouses = List.skip(6)HousesBeforeCapture
                            let newSouthHouses = List.take(6)HousesBeforeCapture
                            let nTup = makeIntoTuple (newNorthHouses,NorthScore)
                            let sTup = makeIntoTuple (newSouthHouses, SouthScore)
                            let (seven, eight, nine, ten, eleven, twelve, SouthScore) = nTup
                            let (one, two, three, four, five, six, NorthScore) = sTup
                            Board(Player(seven, eight, nine, ten, eleven, twelve, NorthScore),  Player(one, two, three, four, five, six, SouthScore), STurn)
                            //Board(NorthHouse, SouthHouse, STurn)
                     | true -> 
                            // 5. NOW THAT SEEDS ARE SOWN, CHECK IF HOUSE WHERE LAST SEED WAS SOWN HAS 2 0R 3 SEEDS AND START CAPTURING AND RECORD SCORE
                            let HousesAfterCapture = setScoreNorth (houseOfLastMove) (numSeeds houseOfLastMove HousesBeforeCapture) (HousesBeforeCapture)
            
                
              
                        (*
                            JUST SOME ADMIN  LINES 190 TO 201
                            AFTER SEEDS HAVE BEEN CAPTURED AND SCORE RECORDED, EACH PLAYER MUST THEN BE REASSINGED TO BOARD
                            'makeIntoTuple' METHOD TAKES EACH PLAYERS HOUSE AND SCORE AND MAKES THEM INTO TUPLE 
                            THE NEW BOARD WITH UPDATED HOUSES AND SCORE IS THEN RETURNED 
                        *)
                            let lst,score1,score2 = HousesAfterCapture
                            let newNorthHouses = List.skip(6)lst
                            let newSouthHouses = List.take(6)lst
                            let addNorth = NorthScore + score2
                            let addSouth = SouthScore + score1
                            let nTup = makeIntoTuple (newNorthHouses,addNorth)
                            let sTup = makeIntoTuple (newSouthHouses, addSouth)
             
                            let (seven, eight, nine, ten, eleven, twelve, addNorth) = nTup //put northscore and check what happens

            
                            let (one, two, three, four, five, six, addSouth) = sTup //non contigious seeds are not captured should fail or not ???
                            match addNorth > 24 with 
                            | true -> Board(Player(seven, eight, nine, ten, eleven, twelve, addNorth),  Player(one, two, three, four, five, six, addSouth), NWin)
                            | false -> Board(Player(seven, eight, nine, ten, eleven, twelve, addNorth),  Player(one, two, three, four, five, six, addSouth), STurn)
                    
            | false ->Board(NorthHouse, SouthHouse, NTurn) //board
  
   | STurn -> 
        let housesToPlay = checkHouses(List.take(6)HouseList)
        match housesToPlay with
        | 0 -> (Board (NorthHouse, SouthHouse, NTurn))
        | _ -> 
            match n >= 1 && n <= 6 with 
            | true -> 
                 let findSeeds house seeds = 
                    let rec search house seeds =
                      match seeds = 0 with 
                      | true -> 
                            match house = 6 with
                            | true -> 
                                     let house = 1 
                                     search house (getSeeds house board)
                            | false -> search (house+1) (getSeeds (house+1) (board))
                      | false -> seeds 
                    search house seeds 
                 let tryThis = checkHouses (List.take(6)HouseList)
                 match tryThis > 0 && getSeeds n board = 0 with //you cannot sow from an empty house
                 | true -> Board(NorthHouse, SouthHouse, STurn)
                 | _ -> 
                     let numOfSeeds = findSeeds n (getSeeds n board)
             
                     let houses = removeSeeds n numOfSeeds HouseList
                     // 4. SOW ONE SEED TO EACH SUCCESSIVE HOUSE IN COUNTERCLOCKWISE MANNER
                     let HousesAfterSowing = distribute n (numOfSeeds) (houses)
             
                     let HousesBeforeCapture,houseOfLastMove = HousesAfterSowing //returns list with seeds added and house where last seed was sown 
                     match houseOfLastMove > 6 && ((numSeeds houseOfLastMove HousesBeforeCapture) = 2 || (numSeeds houseOfLastMove HousesBeforeCapture) = 3)  with 
                     | false -> 
                            let newNorthHouses = List.skip(6)HousesBeforeCapture
                            let newSouthHouses = List.take(6)HousesBeforeCapture
                            let nTup = makeIntoTuple (newNorthHouses,NorthScore)
                            let sTup = makeIntoTuple (newSouthHouses, SouthScore)
             
                            let (seven, eight, nine, ten, eleven, twelve, SouthScore) = nTup

            
                            let (one, two, three, four, five, six, NorthScore) = sTup
             
                            Board(Player(seven, eight, nine, ten, eleven, twelve, NorthScore),  Player(one, two, three, four, five, six, SouthScore), NTurn)

                     | true -> 
                   
                         // 5. NOW THAT SEEDS ARE SOWN, CHECK IF HOUSE WHERE LAST SEED WAS SOWN HAS 2 0R 3 SEEDS AND START CAPTURING AND RECORD SCORE
                         let HousesAfterCapture = setScoreSouth (houseOfLastMove) (numSeeds houseOfLastMove HousesBeforeCapture) (HousesBeforeCapture)

                         (*
                         JUST SOME ADMIN  LINES 190 TO 201
                         AFTER SEEDS HAVE BEEN CAPTURED AND SCORE RECORDED, EACH PLAYER MUST THEN BE REASSINGED TO BOARD
                         'makeIntoTuple' METHOD TAKES EACH PLAYERS HOUSE AND SCORE AND MAKES THEM INTO TUPLE 
                         THE NEW BOARD WITH UPDATED HOUSES AND SCORE IS THEN RETURNED 
                        *)
                         let lst,score1,score2 = HousesAfterCapture //score1 is SouthScore score2 is NorthScore
                         let newNorthHouses = List.skip(6)lst
                         let newSouthHouses = List.take(6)lst
                         let addNorth = NorthScore + score2
                         let addSouth = SouthScore + score1
                         let nTup = makeIntoTuple (newNorthHouses,addNorth)
                         let sTup = makeIntoTuple (newSouthHouses, addSouth)
             
                         let (seven, eight, nine, ten, eleven, twelve, addNorth) = nTup

            
                         let (one, two, three, four, five, six, addSouth) = sTup
                         match addSouth > 24 with 
                         | true ->  Board(Player(seven, eight, nine, ten, eleven, twelve, addNorth),  Player(one, two, three, four, five, six, addSouth), SWin)
                         | false ->  Board(Player(seven, eight, nine, ten, eleven, twelve, addNorth),  Player(one, two, three, four, five, six, addSouth), NTurn)
               
            | false -> Board(NorthHouse, SouthHouse, STurn) //board

   | Draw -> Board(NorthHouse, SouthHouse, Draw)
   | SWin -> Board(NorthHouse, SouthHouse, SWin)
   | NWin -> Board(NorthHouse, SouthHouse, NWin)



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
  | STurn -> "South's turn"
  | Draw -> "Game ended in a draw"
  | SWin -> "South Won"
  | NWin -> "North Won"
   
[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
