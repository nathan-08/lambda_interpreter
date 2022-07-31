-- λ
readdat = readFile "input.dat"

rstrip str = 
  p (reverse str)
  where p [] = []
        p (' ':xs) = p xs
        p ('\n':xs) = p xs
        p rest = (reverse rest)

data Lexeme = Lambda
            | LParen
            | RParen
            | Space
            | Name [Char]
            | Dot
            deriving (Show)

split_word [] = ([], [])
split_word str =
  let f [] a = a
      f ('.':xs) (a, b) = (a, '.':xs)
      f (c:xs) (a, b) = f xs (c:a, b)
      (a, b) = f str ([], [])
  in (reverse a, b)

lexfn [] = []
lexfn ('L':xs) = Lambda : (lexfn xs)
lexfn ('.':xs) = Dot    : (lexfn xs)
lexfn ('(':xs) = LParen : (lexfn xs)
lexfn (')':xs) = RParen : (lexfn xs)
lexfn (' ':xs) = Space  : (lexfn xs)
lexfn (x:xs) =
  let (first, rest) = split_word xs
      name = x : first
  in Name name : (lexfn rest)





