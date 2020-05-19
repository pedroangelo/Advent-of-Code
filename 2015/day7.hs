import Data.Bits
import Data.Char

type Signal = Int
type Wire = String
data Input = Signal Signal | Wire Wire deriving Show
type SignalMap = [Signal]

data Instruction
  = Link Input Wire
  | And Input Input Wire
  | Or Input Input Wire
  | Not Input Wire
  | LShift Input Int Wire
  | RShift Input Int Wire
  deriving Show

type Circuit = [Instruction]

-- GATES

andGate :: Signal -> Signal -> Signal
andGate s1 s2 = (.&.) s1 s2

orGate :: Signal -> Signal -> Signal
orGate s1 s2 = (.|.) s1 s2

notGate :: Signal -> Signal
notGate s = 65535 - s

lshiftGate :: Signal -> Int -> Signal
lshiftGate s ammount = shift s ammount

rshiftGate :: Signal -> Int -> Signal
rshiftGate s ammount = shift s (-ammount)

-- PARSING AND MISC

-- add value to list in position index
addToList :: [a] -> Int -> a -> [a]
addToList list index value = let (l1, l2) = splitAt index list in l1 ++ [value] ++ (tail l2)

-- input is either a Wire (String) or Signal (Int)
parseInput :: String -> Input
parseInput s
  | all isDigit s = Signal $ read s
  | all isLetter s = Wire $ read $ show s

-- parse instruction, exploiting the unique structure of the instruction string
parseInstruction :: String -> Instruction
parseInstruction string
  | instruction !! 1 == "->" = Link (parseInput $ instruction !! 0) (instruction !! 2)
  | instruction !! 1 == "AND" = And (parseInput $ instruction !! 0) (parseInput $ instruction !! 2) (instruction !! 4)
  | instruction !! 1 == "OR" = Or (parseInput $ instruction !! 0) (parseInput $ instruction !! 2) (instruction !! 4)
  | instruction !! 0 == "NOT" = Not (parseInput $ instruction !! 1) (instruction !! 3)
  | instruction !! 1 == "LSHIFT" = LShift (parseInput $ instruction !! 0) (read $ instruction !! 2) (instruction !! 4)
  | instruction !! 1 == "RSHIFT" = RShift (parseInput $ instruction !! 0) (read $ instruction !! 2) (instruction !! 4)
  where instruction = words string

-- WIRES AND CIRCUIT

-- map characters to their position in the alphabet
charToIndex :: Char -> Int
charToIndex c = ord c - 96

-- convert the wire identifier (string) into a int (index) which will refer to the position in the signalMap
wireToIndex :: Wire -> Int
wireToIndex w
  | length w == 1 = charToIndex (w !! 0) -1
  | length w == 2 = (charToIndex (w !! 0) * 26) + charToIndex (w !! 1) -1

-- get the signal from an input
getSignal :: Input -> SignalMap -> Signal
getSignal (Signal signal) signalMap = signal
getSignal (Wire wire) signalMap = {-if wire == "b" then 46065 else-} signalMap!!(wireToIndex wire)

-- check if a signal has reached a certain input
hasSignal :: Input -> SignalMap -> Bool
hasSignal (Signal signal) signalMap = True
hasSignal (Wire wire) signalMap = (getSignal (Wire wire) signalMap) /= -1

-- check if a signal has reached every input in the instruction
instructionHasSignal :: Instruction -> SignalMap -> Bool
instructionHasSignal (Link input wire) signalMap = hasSignal input signalMap
instructionHasSignal (And input1 input2 wire) signalMap = hasSignal input1 signalMap && hasSignal input2 signalMap
instructionHasSignal (Or input1 input2 wire) signalMap = hasSignal input1 signalMap && hasSignal input2 signalMap
instructionHasSignal (Not input wire) signalMap = hasSignal input signalMap
instructionHasSignal (LShift input shift wire) signalMap = hasSignal input signalMap
instructionHasSignal (RShift input shift wire) signalMap = hasSignal input signalMap

-- update signal to wire in signalMap
updateSignal :: SignalMap -> Signal -> Wire -> SignalMap
updateSignal signalMap signal wire = addToList signalMap (wireToIndex wire) signal

-- take the signals from the input, apply the logic operation and add the result to the signalMap
processInstruction :: Instruction -> SignalMap -> SignalMap
processInstruction (Link input wire) signalMap = updateSignal signalMap (getSignal input signalMap) wire
processInstruction (And input1 input2 wire) signalMap = updateSignal signalMap (andGate (getSignal input1 signalMap) (getSignal input2 signalMap)) wire
processInstruction (Or input1 input2 wire) signalMap = updateSignal signalMap (orGate (getSignal input1 signalMap) (getSignal input2 signalMap)) wire
processInstruction (Not input wire) signalMap = updateSignal signalMap (notGate (getSignal input signalMap)) wire
processInstruction (LShift input shift wire) signalMap = updateSignal signalMap (lshiftGate (getSignal input signalMap) shift) wire
processInstruction (RShift input shift wire) signalMap = updateSignal signalMap (rshiftGate (getSignal input signalMap) shift) wire

-- apply several instructions to the signalMap in succession
processReadyInstructions :: [Instruction] -> SignalMap -> SignalMap
processReadyInstructions [] signalMap = signalMap
processReadyInstructions instructions signalMap = processReadyInstructions (tail instructions) $ processInstruction (head instructions) signalMap

-- calculate the result for every instruction and update the result corresponing to the wire in the signal map, calculate instructions in batch, only those whose inputs are already available
buildCircuit :: Circuit -> SignalMap -> SignalMap
buildCircuit [] signalMap = signalMap
buildCircuit circuit signalMap = buildCircuit notReadyInstructions newSignalMap
  where readyInstructions = filter (\x -> instructionHasSignal x signalMap) circuit
        notReadyInstructions = filter (\x -> not $ instructionHasSignal x signalMap) circuit
        newSignalMap = processReadyInstructions readyInstructions signalMap

main :: IO ()
main = do
  putStr "Filepath: "
  filePath <- getLine
  fileContents <- readFile filePath
  let circuit = map parseInstruction $ lines fileContents
  let signalMap = replicate (wireToIndex "zz") $ -1
  let finishedSignalMap = buildCircuit circuit signalMap
  putStrLn $ show finishedSignalMap
