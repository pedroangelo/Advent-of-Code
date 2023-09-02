import Data.Char (isUpper, isLower)
import Data.List (elemIndices, nub, splitAt)
import IOHandler

type Atom = String
type Molecule = [Atom]
type Position = Int
type Replacement = (Atom, Molecule)
type Rules = [Replacement]

addToList :: [a] -> Int -> [a] -> [a]
addToList list index sublist = let (l1, l2) = splitAt index list in l1 ++ sublist ++ tail l2

-- PARSING INPUT

-- parse string into molecule
parseMolecule :: String -> Molecule
parseMolecule "" = []
parseMolecule string = firstAtom : parseMolecule remainingAtoms
          -- an atom starts with an upper case, and ends before the start of another uppercase
    where firstAtom = (head string) : (takeWhile isLower $ tail string)
          remainingAtoms = (dropWhile isLower $ tail string)

-- parse replacements of molecules
parseReplacements :: String -> Replacement
parseReplacements string = (sourceAtom, targetMolecule)
    where stringWords = words string
          sourceAtom = stringWords!! 0
          targetMolecule = parseMolecule $ stringWords !! 2

-- SOLVING FIRST STAR

-- perform one replacement for each valid position in a molecule
replaceMolecule :: Replacement -> Molecule -> [Molecule]
replaceMolecule (atom, replacement) molecule
    -- if atom is not present in molecule, no replacement is made
    | not $ elem atom molecule = []
    -- replace atom with replacement molecule in molecule
    | otherwise = map (\pos -> addToList molecule pos replacement) positions
    -- get positions of the atom in the molecule
    where positions = elemIndices atom molecule

-- SOLVING SECOND STAR

-- start with the full molecule,
-- apply reversed replacements wherever they may fit,
-- obtain new list of molecules from all replacements applied in various positions in the molecule
-- repeat process in this new list until e is reached

-- find molecule component in a molecule, returning a list of indices where the occurrences begin
findComponentMolecule :: Molecule -> Molecule -> [Position]
findComponentMolecule componentMolecule molecule = filter (\pos -> componentMolecule == (take l $ drop pos molecule)) positions
--filter (\pos -> and $ map (\index -> moleculeComponent!!index == molecule!!(pos+index)) [0..l-1]) positions
    where
        -- get positions of the first atom in the molecule, then filter to only those that have enough space for the rest of the molecule
        positions = filter (\p -> p + l <= length molecule) $ elemIndices (head componentMolecule) molecule
        l = length componentMolecule

-- generate various molecules, each by one step reverse replacement in different positions
reverseReplace :: Replacement -> Molecule -> [Molecule]
reverseReplace (atom, replacement) molecule = filter (\m -> if length m > 1 && elem "e" m then False else True) molecules
    where
        -- get the positions of the replacement molecules in the molecule
        positions = findComponentMolecule replacement molecule
        l = length replacement
        -- for each  position, remove the replacement molecule and replace with the atom
        molecules = map (\pos -> (take pos molecule) ++ [atom] ++ (drop (pos+l) molecule)) positions

-- perform all possible reverse replacement of molecules
reverseReplaceAll :: Rules -> Molecule -> [Molecule]
reverseReplaceAll rules molecule = nub $ concat $ map (\r -> reverseReplace r molecule) rules

-- perform all possible reverse replacement of molecules for all molecules
reverseStep :: Rules -> [Molecule] -> [Molecule]
reverseStep rules molecules = nub $ concat $ map (reverseReplaceAll rules) molecules

-- repeatedly reverse replacements, until the molecule is just an electron, counting steps as it goes
fullReverse :: Rules -> [Molecule] -> Int
fullReverse rules molecules
    | elem ["e"] molecules = 0
    | otherwise = 1 + (fullReverse rules $ reverseStep rules molecules)

getSecondStar :: Rules -> [Molecule] -> Int -> IO Int
getSecondStar rules molecules steps
    | elem ["e"] molecules = return steps
    | otherwise = do
        let newMolecules = reverseStep rules molecules
        putStrLn $ "Step " ++ (show steps) ++ ": " ++ (show $ length newMolecules) ++ " new molecules created"
        --mapM_ putStrLn $ map concat newMolecules
        _ <- getLine
        getSecondStar rules newMolecules (steps + 1)

-- SOLVING SECOND STAR

-- start from the left components of a molecule, and collect all rules that match
-- match: whenever a rule has the same starting characters in its replacement

type NumSteps = Int

-- state of a single replacement step
-- Leaf shows the target atom, since no more replacements are needed to reach it
-- Node shows:
-- - the target atom and its replacement
-- - processed atoms have already been processed and found
-- -
data ReplacementStep
    = Leaf Atom NumSteps
    | Node Replacement [(ProcessedAtoms, WaitingAtoms)]
    deriving Show

type ProcessedAtoms = [ReplacementStep]
type WaitingAtoms = [Atom]

-- find rules which can potentially perform an inverse replacement on the leftmost atoms
-- the leftmost atom in the replacement matches the left most atom of the molecule
validReplacements :: Molecule -> Rules -> [Replacement]
validReplacements molecule rules = filter ((head molecule ==) . head . snd) rules

-- obtain the leftmost atoms of the molecule that are also present in the replacement
-- example: for molecule CRnCaSi and replacement H => CRnAlAr, obtain CRn
leftmostAtomMatch :: Molecule -> Replacement -> [Atom]
leftmostAtomMatch molecule replacement = map fst $ takeWhile (\pairAtom -> fst pairAtom == snd pairAtom) $ zip molecule (snd replacement)

buildFinishedLine :: Atom -> ReplacementStep
buildFinishedLine atom = Leaf atom 0

-- build initial steps
-- tuple containing replacement, leftmost matching atoms and remaining atoms
buildInitialSteps :: Molecule -> Rules -> [ReplacementStep]
buildInitialSteps molecule rules = let
    -- obtain valid replacement rules
    validReplacementRules = validReplacements molecule rules
    -- obtain the leftmost matching atom for all rules, along with the rest of the molecule
    leftmostAtoms = [Node v [([buildFinishedLine (head molecule)], tail molecule)] | v <- validReplacementRules]
    in leftmostAtoms

-- obtain all possible atoms
getPossibleAtoms :: Rules -> [Atom]
getPossibleAtoms rules = nub $ concat [(fst r) : (snd r) | r <- rules]

-- obtain atoms that have a replacement rule
getProducingAtoms :: Rules -> [Atom]
getProducingAtoms rules = nub $ map fst rules

-- obtain atoms that do not have a replacement rule
getNonProducingAtoms :: Rules -> [Atom]
getNonProducingAtoms rules =
    let allAtoms = getPossibleAtoms rules
        producing = getProducingAtoms rules
    in filter (\a -> not $ elem a producing) allAtoms

-- obtain all reachable atoms from a single atom
allReachableAtoms :: Rules -> Atom -> [Atom]
allReachableAtoms rules atom = allReachableAtoms' rules atom [atom]

-- obtain all reachable atoms from a single atom, with accumulator
allReachableAtoms' :: Rules -> Atom -> [Atom] -> [Atom]
allReachableAtoms' rules atom reached = do
    -- obtain new reachable atoms, so filter the original atom
    let reachable = filter (/= atom) $ reachableAtoms rules atom
    -- filter already reached atoms, obtaining the atoms we wish to search
    let atomsToSearch = filter (\a -> not $ elem a reached) reachable
    -- update reached atoms
    let reached' = reached ++ atomsToSearch
    -- if there's no more atom to seach, we found them all
    if atomsToSearch == [] then reached' else nub $ concat $ map (\a -> allReachableAtoms' rules a (reached' ++ [a])) atomsToSearch

-- obtain reachable atoms from a single atom
reachableAtoms :: Rules -> Atom -> [Atom]
reachableAtoms rules atom = let
    -- obtain replacement rules that originate from the atom
    validReplacements = filter (\r -> fst r == atom) rules
    -- obtain reached atoms
    newAtoms = nub $ concat $ map snd validReplacements
    in newAtoms

-- CONVERT RULES TO VECTORS, ADD VARIABLES AND LET lp_solve gnu SOLVE THE EQUATION

-- MAIN

-- build molecule and replacements from input
obtainData :: IO (Molecule, [Replacement])
obtainData = do
    -- GATHER INPUT
    -- print puzzle info and get input from user
    input <- obtainPuzzleInput "2015" "19"
    --let fileContents = "e => H\ne => O\nH => HO\nH => OH\nO => HH\n\nHOHOHO"
    let inputLines = lines input
    let replacementsInput = init $ init inputLines
    let moleculeInput = last inputLines

    -- PARSE INPUT
    -- parse replacements rules (from replacement rules in input)
    let rules = map parseReplacements replacementsInput
    -- parse molecule (from input) into a list of the individual molecules
    let molecule = parseMolecule moleculeInput
    return (molecule, rules)

main :: IO ()
main = do
  -- OBTAIN DATA
  (molecule, rules) <- obtainData

  -- FIRST STAR - How many distinct molecules can be created after all the different ways you can do one replacement on the molecule?
  -- generate new molecules via substitution
  let newMolecules = nub $ concat $ map (\r -> replaceMolecule r molecule) rules
  let firstStar = length newMolecules

  -- SECOND STAR - How many replacements steps to reach the molecule?
  --let secondStar = fullReverse replacements [molecule]
  -- secondStar <- getSecondStar rules [molecule] 0
  -- print puzzle results
  printPuzzleResults firstStar "work in progress"
