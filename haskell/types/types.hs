main = do
    putStrLn $ show $ Person "Stjepan" "Poljak"
    putStrLn $ show $ realPerson
    putStrLn $ show $ person realPerson
    putStrLn $ show $ age realPerson

    if Person "Stjepan" "Poljak" == Person "Stjepan" "Poljak"
    then putStrLn "Those people are equal."
    else return ()

    if Person "Stjepan" "Poljak" /= Person "Amela" "Poljak"
    then putStrLn $ "They are not the same person, " ++
                    "but they belong with each other."
    else return ()

    printTree smallTree

    where realPerson = RealPerson dev 34 Male
          dev = Person "Stjepan" "Poljak"

-- type synonyms                    --
type Name       = String
type Surname    = String

-- type constructor                 --
--  |------|                        --
data Person = Person Name Surname
--           |------|               --
--      data / value constructor    --

instance Show Person where
    show (Person x y) = x ++ " " ++ y

personsEq :: Person -> Person -> Bool
personsEq (Person n1 s1) (Person n2 s2) = n1 == n2 && s1 == s2
-- Note: We couldn't do something like:                     --
-- personsEq (Person n s) (Person n s) = True               --
-- A name can appear only once in a set of pattern bindings --

instance Eq Person where
    p1 == p2 = personsEq p1 p2

-- As value and type constructors live in separate          --
-- namespaces (expressions and declarations, respectively), --
-- they can be named the same.                              --

-- Algebraic data type can have more than one constructor.  --
data Color = RGB (Int, Int, Int)
           | CMYK (Int, Int, Int, Int)
           deriving (Show, Eq)

type Age = Int
data Sex = Male | Female

instance Show Sex where
    show Male = "male"
    show Female = "female"

-- Caveat: record type / record syntax fields have to begin --
-- with small caps.                                         --
data RealPerson = RealPerson { person   :: Person
                             , age      :: Age
                             , sex      :: Sex
                             }

-- This is the same as writing:                             --
-- data RealPerson = RealPerson Person Age Sex              --
-- person :: RealPerson -> Person                           --
-- person (RealPerson p _ _) = p                            --

instance Show RealPerson where
    show (RealPerson p a s) = (show p) ++ ", " ++
                              (show a) ++ " year-old " ++
                              (show s) ++ "."

-- Parametrised type is a polymorphic type.                 --
-- Example:                                                 --
-- data Maybe a = Just a | Nothing                          --

-- Recursive types can be used to make list and tree-like   --
-- types.                                                   --

--                 left subtree                             --
--                   |------|                               --
data Tree a = Node a (Tree a) (Tree a) | Empty
--                 |          |------|                      --
--               data       right subtree                   --
            deriving (Show)

leaf :: String -> Tree String
leaf str = Node str Empty Empty

leftN :: String -> Tree String -> Tree String
leftN str tree = Node str tree Empty

rightN :: String -> Tree String -> Tree String
rightN str tree = Node str Empty tree

smallTree :: Tree String
smallTree = Node "This" (leftN "is" (leaf "a"))
                        (leftN "small" (leaf "tree"))

treeIndent :: String
treeIndent = "|___"

wrap :: String -> String
wrap str = "(" ++ str ++ ")"

printTree :: Tree String -> IO ()
printTree tree = printTreeStep tree "" True

    where printTreeStep :: Tree String -> String -> Bool -> IO ()
          printTreeStep Empty _ _ = return ()
          printTreeStep (Node str left right) prefix trig = do
                putStrLn $ (if trig then ""
                            else prefix ++ treeIndent) ++ wrap str
                printTreeStep left ("    " ++ prefix) False
                printTreeStep right ("    " ++ prefix) False
                return ()
