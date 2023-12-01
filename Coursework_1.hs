import Data.List
import Data.Char (isDigit)
import Data.Ord


------------------------- Merge sort

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2
    
------------------------- Game world types

type Character = String
type Party     = [Character]

type Node      = Int
type Location  = String
type Map       = [(Node,Node)]

data Game      = Over
               | Game Map Node Party [Party]
  deriving (Eq,Show)

type Event     = Game -> Game


testGame :: Node -> Game
testGame i = Game [(0,1)] i ["Russell"] [[],["Brouwer","Heyting"]]


------------------------- Assignment 1: The game world

{-

SOLUTION:

Checks for every connection if the node is contained, returns the other node in the pair if there
is a connection. Gets result by concatenating all connected nodes.

-}

connected :: Map -> Node -> [Node]
connected [] _ = []
connected xs n = concatMap (\(a, b) -> if a == n then [b] else if b == n then [a] else []) xs

{-

SOLUTION:

- Simply append the new pair onto the list.
- Doesn't provide any new connections if self-loop or already included in the map.

-}

connect :: Node -> Node -> Map -> Map
connect _ _ [] = []
connect i j xs = if elem ((i, j)) (xs) then xs else (if (i == j) then xs else (xs ++ [(i, j)]))

{-

SOLUTION:

- Uses a filter which only keeps pairs that are not the nodes supplied.
- Checks if any pair are NOT equal, where no nodes are shared in either connection

-}

disconnect :: Node -> Node -> Map -> Map
disconnect i j xs = filter (\(a, b) -> notEqual (i, j) (a, b)) xs
  where
    notEqual :: (Node, Node) -> (Node, Node) -> Bool
    notEqual (a, b) (c, d) = a /= c && b /= d && b /= c && a /= d
    
{-

SOLUTION:

- Connect together the new parties, sort them using msort to maintain ordering property and to 
remove duplicates.

-}

add :: Party -> Event
add p Over = Over
add p (Game x y ps z) = Game x y (msort (p ++ ps)) z

{-

SOLUTION: 

- Returns a new game with the party added to existing party at node position
- Can access this directly using the node as an index
- So use list comprehension to look over all parties in party list
- If index matches up with node, then concatenate

-}

addAt :: Node -> Party -> Event
addAt n p Over = Over
addAt n p (Game x y k pss) = Game x y k (addAt' n p pss)
  where
    addAt' :: Node -> Party -> [Party] -> [Party]
    addAt' n p pss = [if j == n then (msort (p ++ ps)) else ps | (j, ps) <- zip [0..] pss]

{-

SOLUTION:

- Uses addAt which is supplied with the current player's node (second parameter)

-}

addHere :: Party -> Event
addHere p Over = Over
addHere p (Game x current y z) = addAt current p (Game x current y z)

{-

SOLUTION:

- Filter out all of the characters in p that are in ps

-}
remove :: Party -> Event
remove p Over = Over
remove p (Game x y ps z) = Game x y (remove' p ps) z
  where 
    remove' :: Party -> Party -> Party
    remove' p ps = filter (\x -> not (elem x p)) ps 

{-

SOLUTION:



-}

removeAt :: Node -> Party -> Event
removeAt n p Over = Over
removeAt n p (Game x y k pss) = Game x y k (removeAt' n p pss)
  where
    removeAt' :: Node -> Party -> [Party] -> [Party]
    removeAt' n p pss = [if j == n then [] else ps | (j, ps) <- zip [0..] pss]

{-

SOLUTION:

- Recycling removeAt function but supplying the input party.

-}

removeHere :: Party -> Event
removeHere p Over = Over 
removeHere p (Game x current y z) = removeAt current p (Game x current y z)


------------------------- Assignment 2: Dialogues


data Dialogue = Action  String  Event
              | Branch  (Game -> Bool) Dialogue Dialogue
              | Choice  String  [( String , Dialogue )]

testDialogue :: Dialogue
testDialogue = Branch ( isAtZero )
  (Choice "Russell: Let's get our team together and head to Error." [])
  (Choice "Brouwer: How can I help you?"
    [ ("Could I get a haircut?", Choice "Brouwer: Of course." [])
    , ("Could I get a pint?",    Choice "Brouwer: Of course. Which would you like?"
      [ ("The Segmalt.",     Action "" id)
      , ("The Null Pinter.", Action "" id)]
      )
    , ("Will you join us on a dangerous adventure?", Action "Brouwer: Of course." (add ["Brouwer"] . removeHere ["Brouwer"]))
    ]
  )
 where
  isAtZero (Game _ n _ _) = n == 0


dialogue :: Game -> Dialogue -> IO Game

{-
SOLUTION:

(1) First puts the string into the console
(2) Then simply applies event to game state and returns this appropriately as output.

-}

dialogue game (Action s e) = do
    putStrLn s
    return (e game)

{-
SOLUTION:

(1) If branch condition, then continue to dialogue 1 (a)
(2) else continue to dialogue 2

Make recursive call to dialogues which will branch out until eventually
returning game state (when dialogues have completed).

-}

dialogue game (Branch f a b)
  | f game == True = dialogue game a
  | otherwise = dialogue game b

--condition for no responses, simply show string and return game
dialogue game (Choice s []) = do
  putStrLn s
  return (game)
  
{-
SOLUTION:

(1) Displays string (dialogue text)
(2) Pairs up responses with an index (starting from 1) 
(3) Using monadic map (with _ as we only care about printing, not the result of the map) to print out every pair of (index: option)
(4) Display prompt
(5) Bind output to console to choice
(6) First checks if the choice (as integer) is contained within the indices
If so, then find the dialogue matching with this index, otherwise return the game with no further dialogue
This already satisifies the safety requirement in assignment (4a) since the case of input zero is not one of the 
elements of valid responses.
(7)
(8) Filters only for the index which matches up with the choice
Then selects the head (knowing all indices are unique this will be our single selected response)
Then selects the second value of the second tuple (this contains the Dialogue in our type construction)


-}
dialogue game (Choice s rs) = do
  --(1)
  putStrLn s
  --(2)
  let responses = zip [1..] rs
  --(3)
  mapM_ (printResponses) (responses)
  --(4)
  
  --(5)
  choice <- inputLoop

  --(6)
  if elem (read choice :: Int) (map fst responses)
    then dialogue game (continueDialogue (read choice :: Int) responses)
    else return game
  

  where
    --(7)
    printResponses :: (Show a, Show b) => (a, (b ,c)) -> IO ()
    printResponses (a, (b, _)) = putStrLn $ show a ++ ": " ++ show b
    --(8)
    continueDialogue :: Int -> [(Int, (String, Dialogue))] -> Dialogue
    continueDialogue choice rs = snd $ snd $ head $ filter (\(a, (_, _)) -> a == choice) rs
    --(9)
    inputLoop :: IO String
    inputLoop = do
      putStr ">>"
      choice <- getLine
      --remove all whitespace from the string and return [String]
      let checkChoice = words(choice)
      -- take the head of words (choice) to give String input. Take the head of the string as a character, then check whether this is a numeric character.
      if (not $ isDigit $ (head . head) checkChoice) 
        then inputLoop 
        else return (choice)

 
{-

SOLUTION:

(1) First check whether party exists inside theDialogues (is one 
of the first values in all of the tuples) 
(2) If so, then extract this using a filter which selects the party
in theDialogues which matches up, then chooses the second element of this 
tuple which is the corresponding dialogue
(3) Otherwise return a Dialogue which terminates the game and displays
the required string.

-}
  

findDialogue :: Party -> Dialogue 
--(1)
findDialogue p = if elem (p) (map fst theDialogues) 
  --(2)
  then (head (map (snd)  (filter(\x -> (fst x) == p) theDialogues))) 
  --(3)
  else Choice "There is nothing we can do." []
  
  
  
 


------------------------- Assignment 3: The game loop

{-

SOLUTION:

(1) Prints out the description of the current location indexed by n
(2) Gets all of the locations which are connected to the current location, indexes
them from 1, and then uses monadic map (_ since only care about printing / side effect,
no collected result from the map), to show all of the indexes and the corresponding
locations
(3) Gets the index to count up from, which is the last index of the locationsToTravelTo,
then using monadic map again to display the characters in the player's party (p)
with corresponding indices.
(4) Same again, this time selecting the party corresponding to the current location n
(5)


-}

step :: Game -> IO Game
step Over = return Over
step (Game m n p ps) = do
  -- (1)
  putStrLn $ "You are in the " ++ theDescriptions !! n

  -- (2)
  putStrLn "You can travel to: "
  let locationsToTravelTo = zip [1..] (connected m n)
  mapM_ (\(index, n) -> putStrLn $ show index ++ ": " ++ theLocations !! n) locationsToTravelTo

  -- (3)
  putStrLn "With you are: "
  let startIndex = getNextStartIndex (locationsToTravelTo)
  let charactersWith = zip [startIndex ..] p
  mapM_ (\(index, character) -> putStrLn $ show index ++ ": " ++ character) charactersWith

  --(4)

  --Accounting for the case where the index is actually counting from available locations, since sometimes the current party is empty.
  let secondStartIndex = if (charactersWith == []) then ( getNextStartIndex(locationsToTravelTo) ) else ( getNextStartIndex (charactersWith) )
  let charactersAtLocation = zip [secondStartIndex ..] (ps !! n)
  putStrLn "You can see: "
  mapM_ (\(index, character) -> putStrLn $ show index ++ ": " ++ character) charactersAtLocation
  


  --(5)
  let locationsIndices = [1 .. (startIndex - 1)]
  putStrLn "What will you do: "
  choice <- inputLoop
  --get choices as a list of integers (which can be mapped onto chars or locations)
  let choices = map read (words choice) :: [Int]
  --needs to check if any of the choices are inside locationsIndices
  let dialogueCase = not $ any(`elem` locationsIndices) choices
  case (dialogueCase) of
    --if not a dialogue, then simply return game with the new location
    --will be at the head of the split by words since it's a single input.
    False -> do
      let newLocation = head choices
      return (Game m (newLocation) p ps)
    True -> do
      --this if statement implements (4b) safety feature, exits game if 0 entered.
      if head(choices) == 0
        then return Over
        else do
          --first obtains all characters both in the current party and at the location
          let charactersAvailableForDialogue = charactersWith ++ charactersAtLocation

          let gameWithNewParty = getGameWithNewParty p choices charactersAvailableForDialogue (Game m n p ps)
          dialogue (gameWithNewParty) (findDialogue (getNewPartyFromGame gameWithNewParty))
      


  where
    getNextStartIndex :: [(Int, a)] -> Int
    getNextStartIndex xs = (fst $ head $ reverse xs) + 1
    getGameWithNewParty :: Party -> [Int] -> [(Int, Character)] -> Event
    getGameWithNewParty p cs chs (currentGame) = (map snd $ filter (\(index, character) -> elem index cs) chs) `add` currentGame
    getNewPartyFromGame :: Game -> Party
    getNewPartyFromGame (Game _ _ p _) = p
    inputLoop :: IO String
    inputLoop = do
      putStr ">>"
      choice <- getLine
      let checkChoice = words choice
      --check if all of the strings in choice are digits
      if (foldr (\x acc-> (isDigit $ head x) && acc) True checkChoice)
        then return choice
        else inputLoop
      
      


{-

- Starts the loop with choice of game, in this instance it is the first test game.
- The loop itself takes a game and eventually returns nothing once over.
- If the next step returns game over, can end the game completely.
- If the next step returns a new game state, loop again over this game state by
a call to step.


-}


game :: IO ()
game = do
  loop (testGame 0)
  where 
    loop :: Game -> IO ()
    loop Over = return ()
    loop game = do
      new <- step game 
      loop new




------------------------- Assignment 4: Safety upgrades


------------------------- Assignment 5: Solving the game

data Command  = Travel [Int] | Select Party | Talk [Int]
  deriving Show

type Solution = [Command]


{-

(1) Make a call to helper function talk' which initially takes empty list of selected responses
(2) In the Choice case, number all of the possible responses along with dialogues (omitting strings which would be used for output). Then, for all of the possible responses,
concatenate together all of the talk' branches (can be thought of as a tree of calls to talk') which resulted in an action. Makes sure to maintain state of each call to talk' by
adding the response number to the list of visited dialogues.
(3) In the Action case, return specific game with event applied and list 
(4) In the Branch case, simply make calls to talk' for both different dialogues off of either branch.


-}


talk :: Game -> Dialogue -> [(Game,[Int])]

--(1)
talk game dialogue = talk' game dialogue []
  where 
    talk' :: Game -> Dialogue -> [Int] -> [(Game, [Int])]
    --(2)
    talk' game (Choice _ responses) xs = do
      let new = zip [1..] (map(\(_, b) -> b) responses)
      concatMap (\(a, b) -> talk' game (b) (xs ++ [a])) (new) 

    --(3)
    talk' game (Action _ e) xs = [(e game, xs)]

    --(4)
    talk' game (Branch f a b) xs
      | f game == True = talk' game a xs
      | otherwise = talk' game b xs
    
{-

(1) Should select all of the combinations of both the player's party p and the party currently at the location n
(2) Simply number up all of p's, and match up with corresponding value n (number from 0 since nodes are numbered from 0)
(3) The combinations of both lists is just all of the different subsequences of the two concatenated
(4) Can get all subsequences using the principle of either including or excluding each element. The first case includes element x, and second doesn't.
Then concatenate our results into a final result list.

-}

                                                           
select :: Game -> [Party]
--(1)
select (Game _ n p ps) = combinations (p) (getPartyAtLocation n ps)

  where
    getPartyAtLocation :: Int -> [Party] -> Party
    --(2)
    getPartyAtLocation n ps = snd $ head $ filter (\(a, b) -> a == n) (zip [0..] ps)
    combinations :: Party -> Party -> [Party]
    --(3)
    combinations p1 p2 = selections (p1 ++ p2)
      where
        selections :: Party -> [Party]
        selections [] = [[]]
        --(4)
        selections (x:xs) = [x:ys | ys <- selections xs] ++ selections xs

{-

Solution -

Implementation of recursive DFS, making use of the implicit call stack.

(1) First we make call to helper function which takes of nodes visited along current path AND all nodes visited along any path
(2) The result will be the initial self loop alongside all of the shortest path returned from a call to dfs for the map provided
(3) Implementation of recursive DFS:
  (a) if we find that we have already visited a node, stop calls to DFS (prevents infinite loop due to cycles in the map)
  (b) otherwise, we return the node alongside with the current path to reach it, and make another call to dfs with the new path, and visited including the new node searched.
  ConcatMap ensures that every call to dfs which returns a path and another call to dfs is included in one result list at the end.
(4) Simply gets all of the choices for all connected nodes so we can return what would be user input instead of the Nodes.
(5) Step where we remove any longer paths which would be returned from DFS since it doesn't search earliest first
  (5a) to get all paths in groups to the same node, we groupBy the first value being the same (the node reached). This can be done since we first sort all of the tuples meaning
  that all paths with same first value are contiguous 
  (5b) then we sort again for each sublist based on the length of the second value of tuples
  Then we call to map head to get the shortest path for every group.



-} 

--below is recursive DFS implementation, requires sorting and removing duplicates at the end to get rid of shorter paths

travel :: Map -> Node -> [(Node,[Int])]
--(1)
travel m n = travel' m n [] [n]
 where
  travel' :: Map -> Node -> [Int] -> [Int] -> [(Node, [Int])]
  --(2)
  travel' m n xs visited = [(n, [])] ++ getShortest (dfs m n xs visited)

    where
      dfs :: Map -> Node -> [Int] -> [Int] -> [(Node, [Int])]
      --(3)
      dfs m n xs visited = concatMap (\(a, b) -> if b `elem` visited 
        then []
        else [(b, xs ++ [a])] ++ dfs m b (xs ++ [a]) (visited ++ [b])) (getChoices m n)

      getChoices :: Map -> Node -> [(Int, Node)]
      --(4)
      getChoices m n = zip [1..] (connected m n)

      getShortest :: [(Node, [Int])] -> [(Node, [Int])]
      --(5)
      getShortest xs = do
        --(5a)
        let pathsToNode = groupBy (\(x, _) (y, _) -> x == y) $ msort xs
        --(5b)
        let getShortestPaths = map (head) (map (sortBy (comparing (length . snd))) (pathsToNode))
        --let getShortestPaths = map (head) (map (sortBy (\(_,len1) (_,len2) -> compare len1 len2)) (pathsToNode))
        getShortestPaths



{-

(1) List all of the possible travel sequences using (travel m)
(2) Then for every possible travel sequence, select all parties for that sequence, and talk to those parties.

Solution should always be the list [Travel, Select, Talk]


-}

allSteps :: Game -> [(Solution, Game)]
allSteps (Game m n p ps) = 
 concatMap (\(a, b) -> getSolutions a b (Game m n p ps)) (travel m n)
   where
     getSolutions :: Node -> [Int] -> Game -> [(Solution, Game)]
     getSolutions k t (Game m n p ps) = do
      --get new game state for each possible travel step
      let newGameState = Game m k p ps
      --get all the parties that could be selected 
      let availableParties = select (newGameState)
      --pair up all of the parties that could be selected with their dialogues
      let dialogues = map (\p -> (p, findDialogue p)) (availableParties)
      --pair up all of the parties with the talk result for dialogues
      let talkResults = map (\(p, dialogue) -> (p, talk (newGameState) (dialogue))) dialogues
      --take all solutions as triples of currently travelled path, selected party, and talk reslt
      let solutions = [([Travel t, Select x, Talk y], g) | (x, ys) <- talkResults, (g, y) <- ys]
      solutions
      

solve :: Game -> Solution
solve g = solve' g []
 where
   solve' :: Game -> Solution -> Solution
   solve' g s = 
    --get all of the steps for the current game state
     let steps = allSteps g
     in 
      case steps of
        --return solution if no more steps available
       [] -> s
       --return the first solution (of which there could be a few).
       _ -> head $ map (\(solution, newGameState) -> solve' newGameState (s ++ solution)) steps       
        

      
walkthrough :: IO ()
walkthrough = (putStrLn . unlines . filter (not . null) . map format . solve) start
  where
    format (Travel []) = ""
    format (Travel xs) = "Travel: " ++ unwords (map show xs)
    format (Select xs) = "Select: " ++ foldr1 (\x y -> x ++ ", " ++ y) xs
    format (Talk   []) = ""
    format (Talk   xs) = "Talk:   " ++ unwords (map show xs)


------------------------- Game data

start :: Game
start = Game theMap 1 [] theCharacters

theMap :: Map
theMap = [(1,2),(1,6),(2,4)]


theLocations :: [Location]
theLocations =
  -- Logicester
  [ "Home"           -- 0
  , "Brewpub"        -- 1
  , "Hotel"          -- 2
  , "Hotel room n+1" -- 3
  , "Temple"         -- 4
  , "Back of temple" -- 5
  , "Takeaway"       -- 6
  , "The I-50"       -- 7
  ]

theDescriptions :: [String]
theDescriptions =
  [ "your own home. It is very cosy."
  , "the `Non Tertium Non Datur' Brewpub & Barber's."
  , "the famous Logicester Hilbert Hotel & Resort."
  , "front of Room n+1 in the Hilbert Hotel & Resort. You knock."
  , "the Temple of Linearity, Logicester's most famous landmark, designed by Le Computier."
  , "the back yard of the temple. You see nothing but a giant pile of waste paper."
  , "Curry's Indian Takeaway, on the outskirts of Logicester."
  , "a car on the I-50 between Logicester and Computerborough. The road is blocked by a large, threatening mob."
  ]

theCharacters :: [Party]
theCharacters =
  [ ["Bertrand Russell"]                    -- 0  Home
  , ["Arend Heyting","Luitzen Brouwer"]     -- 1  Brewpub
  , ["David Hilbert"]                       -- 2  Hotel
  , ["William Howard"]                      -- 3  Hotel room n+1
  , ["Jean-Yves Girard"]                    -- 4  Temple
  , []                                      -- 5  Back of temple
  , ["Haskell Curry", "Jean-Louis Krivine"] -- 6  Curry's takeaway
  , ["Gottlob Frege"]                       -- 7  I-50
  ]

theDialogues :: [(Party,Dialogue)]
theDialogues = let
  always _ = True
  end str  = Choice str []
  isconn  _ _  Over           = False
  isconn  i j (Game m _ _ _ ) = elem i (connected m j)
  here         Over           = 0
  here        (Game _ n _ _ ) = n
  inParty   _  Over           = False
  inParty   c (Game _ _ p _ ) = elem c p
  isAt    _ _  Over           = False
  isAt    n c (Game _ _ _ ps) = elem c (ps !! n)
  updateMap _  Over           = Over
  updateMap f (Game m n p ps) = Game (f m) n p ps
 in
  [ ( ["Russell"] , Choice "Russell: Let's go on an adventure!"
      [ ("Sure." , end "You pack your bags and go with Russell.")
      , ("Maybe later.", end "Russell looks disappointed.")
      ]
    )
  , ( ["Heyting","Russell"] , end "Heyting: Hi Russell, what are you drinking?\nRussell: The strong stuff, as usual." )
  , ( ["Bertrand Russell"] , Branch (isAt 0 "Bertrand Russell") ( let
      intro = "A tall, slender, robed character approaches your home. When he gets closer, you recognise him as Bertrand Russell, an old friend you haven't seen in ages. You invite him in.\n\nRussell: I am here with a important message. The future of Excluded-Middle Earth hangs in the balance. The dark forces of the Imperator are stirring, and this time, they might not be contained.\n\nDo you recall the artefact you recovered in your quest in the forsaken land of Error? The Loop, the One Loop, the Loop of Power? It must be destroyed. I need you to bring together a team of our finest Logicians, to travel deep into Error and cast the Loop into lake Bottom. It is the only way to terminate it."
      re1   = ("What is the power of the Loop?" , Choice "Russell: for you, if you put it on, you become referentially transparent. For the Imperator, there is no end to its power. If he gets it in his possession, he will vanquish us all." [re2])
      re2   = ("Let's go!" , Action "Let's put our team together and head for Error." (updateMap (connect 1 0) . add ["Bertrand Russell"] . removeHere ["Bertrand Russell"]) )
      in Choice intro [re1,re2]
      ) ( Branch ( (==7).here) (end "Russell: Let me speak to him and Brouwer."
      ) (end "Russell: We should put our team together and head for Error." ) )
    )
  , ( ["Arend Heyting"] , Choice "Heyting: What can I get you?"
      [ ( "A pint of Ex Falso Quodbibet, please." , end "There you go." )
      , ( "The Hop Erat Demonstrandum, please."   , end "Excellent choice." )
      , ( "Could I get a Maltus Ponens?"          , end "Mind, that's a strong one." )
      ]
    )
  , ( ["Luitzen Brouwer"] , Branch (isAt 1 "Luitzen Brouwer")
      ( Choice "Brouwer: Haircut?"
        [ ( "Please." , let
          intro = "Brouwer is done and holds up the mirror. You notice that one hair is standing up straight."
          r1 i  = ( "There's just this one hair sticking up. Could you comb it flat, please?" , d i)
          r2    = ( "Thanks, it looks great." , end "Brouwer: You're welcome.")
          d  i  | i == 0    = Choice intro [r2]
                | otherwise = Choice intro [r1 (i-1),r2]
        in d 100)
        , ( "Actually, could you do a close shave?" , end "Of course. I shave everyone who doesn't shave themselves." )
        , ( "I'm really looking for help." , Choice "Brouwer: Hmmm. What with? Is it mysterious?"
          [ ( "Ooh yes, very. And dangerous." , Action "Brouwer: I'm in!" (add ["Luitzen Brouwer"] . removeHere ["Luitzen Brouwer"]) )
          ] )
        ]
      )
      ( end "Nothing" )
    )
  , ( ["David Hilbert"] , Branch (not . isconn 2 3) (let
        intro = "You wait your turn in the queue. The host, David Hilbert, puts up the first guest in Room 1, and points the way to the stairs.\n\nYou seem to hear that the next couple are also put up in Room 1. You decide you must have misheard. It is your turn next.\n\nHilbert: Lodging and breakfast? Room 1 is free."
        re1   = ("Didn't you put up the previous guests in Room 1, too?" , Choice "Hilbert: I did. But everyone will move up one room to make room for you if necessary. There is always room at the Hilbert Hotel & Resort." [("But what about the last room? Where do the guests in the last room go?" , Choice "Hilbert: There is no last room. There are always more rooms." [("How can there be infinite rooms? Is the hotel infinitely long?" , Choice "Hilbert: No, of course not! It was designed by the famous architect Zeno Hadid. Every next room is half the size of the previous." [re2])])])
        re2   =  ("Actually, I am looking for someone." , Action "Hilbert: Yes, someone is staying here. You'll find them in Room n+1. Through the doors over there, up the stairs, then left." (updateMap (connect 2 3)))
      in Choice intro [re1,re2]
      ) (end "Hilbert seems busy. You hear him muttering to himself: Problems, problems, nothing but problems. You decide he has enough on his plate and leave." )
    )
  , ( ["William Howard"] ,  Branch (isAt 3 "William Howard")
      (Choice "Howard: Yes? Are we moving up again?" [("Quick, we need your help. We need to travel to Error." , Action "Howard: Fine. My bags are packed anyway, and this room is tiny. Let's go!" (add ["William Howard"] . removeAt 3 ["William Howard"]))]
      ) (Branch (isAt 6 "William Howard") (Choice "Howard: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Howard: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        ]
      ) (end "Howard: We need to find Curry. He'll know the way.")
    ) )
  , ( ["Jean-Yves Girard"] , Branch (isconn 4 5)  (end "You have seen enough here.") (Action "Raised on a large platform in the centre of the temple, Girard is preaching the Linearity Gospel. He seems in some sort of trance, so it is hard to make sense of, but you do pick up some interesting snippets. `Never Throw Anything Away' - you gather they must be environmentalists - `We Will Solve Church's Problems', `Only This Place Matters'... Perhaps, while he is speaking, now is a good time to take a peek behind the temple..." (updateMap (connect 4 5) ))
    )
  , ( ["Vending machine"] , Choice "The walls of the Temple of Linearity are lined with vending machines. Your curiosity gets the better of you, and you inspect one up close. It sells the following items:"
      [ ( "Broccoli"  , end "You don't like broccoli." )
      , ( "Mustard"   , end "It might go with the broccoli." )
      , ( "Watches"   , end "They seem to have a waterproof storage compartment. Strange." )
      , ( "Camels"    , end "You don't smoke, but if you did..." )
      , ( "Gauloises" , end "You don't smoke, but if you did..." )
      ]
    )
  , ( ["Jean-Louis Krivine"] , end "Looking through the open kitchen door, you see the chef doing the dishes. He is rinsing and stacking plates, but it's not a very quick job because he only has one stack. You also notice he never passes any plates to the front. On second thought, that makes sense - it's a takeaway, after all, and everything is packed in cardboard boxes. He seems very busy, so you decide to leave him alone."
    )
  , ( ["Haskell Curry"] , Branch (isAt 6 "Haskell Curry")
      (Choice "Curry: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Curry: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        , ("Actually, I am looking for help getting to Error." , end "Curry: Hmm. I may be able to help, but I'll need to speak to William Howard.")
        ]
      ) (end "Nothing")
    )
  , ( ["Haskell Curry","William Howard"] , Branch (not . isconn 6 7) (Action "Curry:  You know the way to Error, right?\nHoward: I thought you did?\nCurry:  Not really. Do we go via Computerborough?\nHoward: Yes, I think so. Is that along the I-50?\nCurry:  Yes, third exit. Shall I go with them?\nHoward: sure. I can watch the shop while you're away." (add ["Haskell Curry"] . removeAt 6 ["Haskell Curry"] . addAt 6 ["William Howard"] . remove ["William Howard"] . updateMap (connect 6 7) )) (end "It's easy, just take the third exit on I-50.")
    )
  , ( ["Gottlob Frege"] , end "A person who appears to be the leader of the mob approaches your vehicle. When he gets closer, you recognise him as Gottlob Frege. You start backing away, and he starts yelling at you.\n\nFrege: Give us the Loop! We can control it! We can wield its power!\n\nYou don't see a way forward. Perhaps Russell has a plan." )
  , ( ["Bertrand Russell","Gottlob Frege","Luitzen Brouwer"] , let
        intro = "Frege is getting closer, yelling at you to hand over the Loop, with the mob on his heels, slowly surrounding you. The tension in the car is mounting. But Russell calmly steps out to confront Frege.\n\nRussell:"
        re1   = ( "You cannot control its power! Even the very wise cannot see all ends!" , Choice "Frege: I can and I will! The power is mine!\n\nRussell:" [re2,re3] )
        re2   = ( "Brouwer, whom do you shave?" , Choice "Brouwer: Those who do not shave themselves. Obviously. Why?\n\nRussell:" [re3] )
        re3   = ( "Frege, answer me this: DOES BROUWER SHAVE HIMSELF?" , Action
                  "Frege opens his mouth to shout a reply. But no sound passes his lips. His eyes open wide in a look of bewilderment. Then he looks at the ground, and starts walking in circles, muttering to himself and looking anxiously at Russell. The mob is temporarily distracted by the display, uncertain what is happening to their leader, but slowly enclosing both Frege and Russell. Out of the chaos, Russell shouts:\n\nDRIVE, YOU FOOLS!\n\nYou floor it, and with screeching tires you manage to circle around the mob. You have made it across.\n\nEND OF ACT 1. To be continued..."
                  (const Over)
                )
      in Choice intro [re1,re2,re3]
    )
  , ( ["Bertrand Russell","Haskell Curry","Luitzen Brouwer"] , Branch ((==7).here) (end "Road trip! Road trip! Road trip!") (end "Let's head for Error!")
    )
  ]