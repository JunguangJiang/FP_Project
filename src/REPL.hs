module REPL where
import Parser
import AST
import EvalType
import EvalValue
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec

-- single line mode
line :: IO ()
line = do
    input <- getLine
    REPL.eval (input++";")

-- multiline mode
lines :: IO ()
lines = do
    input <- getLines
    REPL.eval input

-- eval a string to its value and type
eval :: String -> IO ()
eval input =
    case parse Parser.program "" input of
        Left err -> putStr $ errorBundlePretty err
        Right result -> mapM_ (
            \x -> do
                putStr $ show $ EvalValue.evalValue x
                putStr " :: "
                print $ EvalType.evalType x
            ) result