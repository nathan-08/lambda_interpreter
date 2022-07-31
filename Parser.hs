import Lexer

data Node = NameNode [Char]  
          | Function Node Node
          | Expression Node
          | Application Node Node
          | FnExpression Node
          | ArgExpression Node
          deriving (Show)
-- parse list of Lexemes into AST
expr ((Name name):xs) = (Just (Expression (NameNode name)), xs)
expr (lexms) =
    let (func, rest) = function(lexms)
    in
      case func of
      Just f -> (Just (Expression f), rest)
      Nothing -> 
           let (maybe_app, rest) = application(lexms)
           in case maybe_app of
             Just app -> (Just (Expression app), rest)
             Nothing -> (Nothing, [])
--expr xs = (Nothing, [])

function (Lambda:(Name name):Dot:xs) =
  let (maybe_exp, rest) = expr(xs)
  in case maybe_exp of
    Just exp -> (Just (Function (NameNode name) exp), rest)
    Nothing -> (Nothing, [])
function _ = (Nothing, [])

application (LParen:xs) = 
  let (maybe_fexpr, rest) = expr(xs)
  in case maybe_fexpr of
    Just fexpr -> 
      let (maybe_argexpr, rest') = expr(tail rest)
      in case maybe_argexpr of
        Just argexpr -> if isSpace (head rest) then (Just (Application fexpr argexpr), tail rest') else (Nothing, [])
        Nothing -> (Nothing, [])
    Nothing -> (Nothing, [])
application _ = (Nothing, [])



