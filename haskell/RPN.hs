-- td1
--
-- CALCULATRICE RPN
--
--
module RPN where 

import System.IO
import Peano

-- définition du type Stack
--type Stack = [Integer]
type Stack = [Peano]

-- défintion du type Operator
type Operator = Stack -> Stack

-- définition de parseOp

parseOp :: String -> Operator
parseOp "+"     = (\stack -> [(+) (stack !! 0) (stack !! 1)] ++ (drop 2 stack))
parseOp "-"     = (\stack -> [(-) (stack !! 1) (stack !! 0)] ++ (drop 2 stack))
parseOp "*"     = (\stack -> [(*) (stack !! 0) (stack !! 1)] ++ (drop 2 stack))
parseOp "/"     = (\stack -> [(div) (stack !! 1) (stack !! 0)] ++ (drop 2 stack))
parseOp "dup"   = (\stack -> [stack !! 0] ++ stack)
parseOp "swap"  = (\stack -> [(stack !! 1), (stack !! 0)] ++ (drop 2 stack))
parseOp "drop"  = (\stack -> tail stack)
-- parseOp "depth" = (\stack -> [toInteger (length stack)] ++ stack)
parseOp "depth" = (\stack -> [fromInteger (toInteger (length stack))] ++ stack)
--parseOp "pick"  = (\stack -> [stack !! (fromInteger ((stack !! 0)+1))] ++ (drop 1 stack))
parseOp "pick"  = (\stack -> [stack !! (toEnum (fromInteger ((toInteger ((stack !! 0)))+1)))] ++ (drop 1 stack))
--parseOp x       = (\stack -> [((read x) :: Integer)] ++ stack)
parseOp x       = (\stack -> [((read x) :: Peano)] ++ stack)


-- fonction d'évaluation des opérateurs sur la pile

eval :: Stack -> [Operator] -> Stack
eval stack [] = stack
eval stack (x : xs) = eval (x stack) xs

-- implémentation de parse
parse :: String -> [Operator]
parse s = map (parseOp) (words s)

-- repl

repl :: Stack -> IO ()
repl stack = do
  putStr "<God says> "
  hFlush stdout
  line <- getLine
  newstack <- return $ eval stack (parse line)
  putStrLn $ show $ reverse newstack
  repl newstack
--main = repl []
