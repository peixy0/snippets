import Data.Char
import System.Environment

interpret :: Char -> ([Int], Int) -> IO ([Int], Int)
interpret '<' (stack, sp) = return (stack, sp - 1)
interpret '>' (stack, sp) = return (stack, sp + 1)
interpret '+' (stack, sp) =
    return (before ++ [x + 1] ++ after, sp)
    where (before, x:after) = splitAt sp stack

interpret '-' (stack, sp) =
    return (before ++ [x - 1] ++ after, sp)
    where (before, x:after) = splitAt sp stack

interpret '.' (stack, sp) = do
    putChar . chr $ (stack !! sp)
    return (stack, sp)

interpret ',' (stack, sp) = do
    x <- getChar
    return (before ++ [ord x] ++ after, sp)
    where (before, _:after) = splitAt sp stack

interpret _ (stack, sp) = return (stack, sp)

skip :: [Char] -> [Char]
skip code = skip' code 1

skip' :: [Char] -> Int -> [Char]
skip' code 0 = code
skip' (op:rest) i
    | op == '[' = skip' rest (i + 1)
    | op == ']' = skip' rest (i - 1)
    | otherwise = skip' rest i

loop :: [Char] -> ([Int], Int) -> IO ([Int], Int)
loop code (stack, sp)
    | stack !! sp == 0 = run (skip code) (stack, sp)
    | otherwise        = do (newStack, newSp) <- run code (stack, sp)
                            loop code (newStack, newSp)

run :: [Char] -> ([Int], Int) -> IO ([Int], Int)
run [] (stack, sp) = return (stack, sp)
run code@(op:rest) (stack, sp)
    | op == '[' = loop rest (stack, sp)
    | op == ']' = return (stack, sp)
    | otherwise = do (newStack, newSp) <- interpret op (stack, sp)
                     run rest (newStack, newSp)

main :: IO ()
main = do
    file <- fmap head getArgs
    code <- readFile file
    run code (repeat 0, 0)
    return ()
