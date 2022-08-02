module Parser
( parse
, beta_reduce
, to_string
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
beta_reduce :: Expression -> (Bool, Expression)

beta_reduce (Application app@(Application _ _) arg) =
  let (b, app') = beta_reduce app
      (b', arg') = beta_reduce arg
  in if b || b' then beta_reduce (Application app' arg')
  else (False, Application app' arg')

beta_reduce (Application (Function parm body) arg) =
  (True, snd(beta_reduce (apply parm arg body)))

beta_reduce (Function parm body) =
  let (b, body') = beta_reduce body
  in (b, Function parm body')
  
beta_reduce (Application n@(NameExpr _) arg) =
  let (b, arg') = beta_reduce arg
  in (b, Application n arg')

beta_reduce n@(NameExpr _) = (False, n)

apply :: String -> Expression -> Expression -> Expression
apply parm arg body =
  case body of
    NameExpr name -> if name == parm then arg else body

    Function p b ->
      Function p (apply parm arg b)

    Application e1 e2 ->
      (Application
        (apply parm arg e1)
        (apply parm arg e2))

to_string :: Expression -> String
to_string (NameExpr name) = name
to_string (Function parm body)=
  "λ" ++ parm ++ "." ++ (to_string body)
to_string (Application e1 e2) =
  "(" ++ (to_string e1) ++ " " ++ (to_string e2) ++ ")"
