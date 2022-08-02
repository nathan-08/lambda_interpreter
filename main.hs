import Lexer
import Parser

main :: IO ()
main = do
  t <- readdat
  let t' = rstrip t
      lexms = lexfn t'
      maybe_ast = parse lexms
  --putStrLn (show lexms)
      reduced = case maybe_ast of
        Just ast -> Just (snd(beta_reduce ast))
        Nothing -> Nothing
  case reduced of
    Just r -> putStrLn (to_string r)
    Nothing -> print "something went wrong"



