module Parser
( parse
, beta_reduce
) where
import Lexer

data Expression = NameExpr [Char]
                | Function [Char] Expression
                | Application Expression Expression
                deriving (Show)

parse (lxms) = fst (expression lxms)
-- AST building functions
-- <expression> = <name> | <function> | <application>
expression ((Name name):xs) = (Just (NameExpr name), xs)
expression (lexms) =
  let (maybe_func, rest) = function lexms
  in case maybe_func of
    Just func -> (maybe_func, rest)
    Nothing   ->
      let (maybe_app, rest) = application lexms
      in case maybe_app of
        Just app -> (maybe_app, rest)
        Nothing -> (Nothing, [])

-- <function> = λ<name>.<expression>
function (Lambda:(Name name):Dot:xs) =
  let (maybe_exp, rest) = expression xs
  in case maybe_exp of
    Just exp -> (Just (Function name exp), rest)
    Nothing -> (Nothing, [])
function _ = (Nothing, [])

-- <application> = (<function expresson> <argument expression>)
application (LParen:xs) =
  let (maybe_fexpr, rest) = expression xs
      maybe_space = head rest
      (maybe_argexpr, rest') = expression (tail rest)
      maybe_RParen = head rest'
      rest'' = tail rest'
  in case maybe_fexpr of
    Nothing -> (Nothing, [])
    Just fexpr ->
      case maybe_argexpr of
        Nothing -> (Nothing, [])
        Just argexpr ->
          if isSpace maybe_space && isRParen maybe_RParen
          then (Just (Application fexpr argexpr), rest'')
          else (Nothing, [])

application _ = (Nothing, [])

--beta reduction
--find and eliminate Application expressions
beta_reduce :: Expression -> Expression
beta_reduce (Application (Function parm body) arg) =
  case arg of
    Application _ _ -> apply parm (beta_reduce arg) body
    _ -> apply parm arg body

beta_reduce x = x
  
apply :: String -> Expression -> Expression -> Expression
apply parm arg body =
  --replace instances of NameExpr parm in body with arg
  case body of
    NameExpr name -> if name == parm then arg else body

    Function p b ->
      Function p (apply parm arg b)

    Application e1 e2 ->
      beta_reduce (Application
            (apply parm arg e1)
            (apply parm arg e2))

