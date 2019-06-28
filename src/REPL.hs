module REPL where
import Parser
import AST
import EvalType
import EvalValue
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec

main :: IO ()
main = getAST True

eval :: IO ()
eval = do
    input <- getLines
    case parse Parser.program "" input of
        Left err -> putStr $ errorBundlePretty err
        Right result -> mapM_ (
            \x -> do
                putStr $ show $ EvalValue.evalValue x
                putStr " :: "
                putStrLn $ show $ EvalType.evalType x
            ) result