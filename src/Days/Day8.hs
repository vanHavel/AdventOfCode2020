module Days.Day8 where

import Control.Monad.Reader
import Control.Monad.State

data Operation = ACC | JMP | NOP deriving (Eq, Show)
fromString :: String -> Operation 
fromString "acc" = ACC 
fromString "jmp" = JMP 
fromString "nop" = NOP

data Instruction = Instruction Operation Int deriving (Eq, Show)
type Program = [Instruction]
data ProgramState = ProgramState {pc :: Int, acc :: Int}
initialState :: ProgramState
initialState = ProgramState {pc = 0, acc = 0}

type Interpreter = ReaderT Program (State ProgramState)
runInterpreter :: Program -> Interpreter a -> a 
runInterpreter code inter = evalState (runReaderT inter code) initialState

step :: Interpreter ()
step = do 
    i <- gets pc
    (Instruction op num) <- asks (!! i)
    case op of
        NOP -> updatePc 1 
        JMP -> updatePc num
        ACC -> updateAcc num >> updatePc 1

updatePc :: Int -> Interpreter ()
updatePc num = modify (\s -> s{pc = num + pc s})
updateAcc :: Int -> Interpreter ()
updateAcc num = modify (\s -> s{acc = num + acc s})

runTillLoop :: [Int] -> Interpreter Int
runTillLoop seen = do 
    i <- gets pc 
    if i `elem` seen 
        then gets acc
        else step >> runTillLoop (i:seen)

runTillTermination :: [Int] -> Interpreter (Maybe Int)
runTillTermination seen = do 
    i <- gets pc 
    l <- asks length
    if i `elem` seen || i < 0 || i > l
        then return Nothing
        else if i == l 
            then gets (Just . acc)
            else step >> runTillTermination (i:seen)
        
run :: String -> String 
run s = 
    let program = map parse $ lines s 
        res1 = runInterpreter program (runTillLoop [])
        res2 = maximum [runInterpreter (change i program) (runTillTermination []) | i <- [0..(pred $ length program)]]
    in show res1 ++ ", " ++ show res2

change :: Int -> Program -> Program 
change i program = 
    let (before, line:after) = splitAt i program 
    in before ++ (changeOp line : after)

changeOp :: Instruction -> Instruction
changeOp (Instruction op num) = 
    let newop = case op of
            JMP -> NOP 
            NOP -> JMP 
            ACC -> ACC
    in Instruction newop num

parse :: String -> Instruction 
parse s = let [op, num] = words $ filter (/= '+') s 
          in Instruction (fromString op) (read num)