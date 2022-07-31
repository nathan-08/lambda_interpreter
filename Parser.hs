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
    let (maybe_func, rest) = function(lexms)
    in case maybe_func of
      Just func -> (Just (Expression func), rest)
      Nothing -> 
        let (maybe_app, rest) = application(lexms)
        in case maybe_app of
          Just app -> (Just (Expression app), rest)
          Nothing -> (Nothing, [])

function (Lambda:(Name name):Dot:xs) =
  let (maybe_exp, rest) = expr(xs)
  in case maybe_exp of
    Just exp -> (Just (Function (NameNode name) exp), rest)
    Nothing -> (Nothing, [])
function _ = (Nothing, [])

application (LParen:xs) = 
  let (maybe_fexpr, rest) = expr(xs)
  in case maybe_fexpr of
    Nothing -> (Nothing, [])
    Just fexpr -> 
      let (maybe_argexpr, rest') = expr(tail rest)
      in case maybe_argexpr of
        Nothing -> (Nothing, [])
        Just argexpr -> if isSpace (head rest)
                        then (Just (Application fexpr argexpr), tail rest')
                        else (Nothing, [])
application _ = (Nothing, [])


apply parm arg body =
  --replace instances of NameNode parm in body with arg
  case body of
    Expression(NameNode name) -> if name == parm then arg else body

    Expression(Function p b) ->
      Expression (Function p (apply parm arg b))

    Expression(Application (Expression (Function p' fbod)) abod) ->
      beta_reduce (Expression(
          Application
            (Expression (Function p' (apply parm arg fbod)))
            (apply parm arg abod)))

    Expression(Application e1 e2) ->
      beta_reduce (Expression(
            Application
            (apply parm arg e1)
            (apply parm arg e2)
            ))

--beta reduction
--traverse tree top down to find and eliminate application nodes
beta_reduce :: Node -> Node
beta_reduce (Expression
    (Application (Expression (Function (NameNode parm) body)) arg)) =
  case arg of
    (Expression (Application _ _) ) ->
      (apply parm (beta_reduce (Expression arg)) body)
    (Expression _) -> (apply parm arg body)

beta_reduce x = x
  
