main = do
    putStrLn $ show $ Person "Stjepan" "Poljak"
    putStrLn $ show $ realPerson
    putStrLn $ show $ person realPerson
    putStrLn $ show $ age realPerson

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

left :: String -> Tree String -> Tree String
left str tree = Node str tree Empty

right :: String -> Tree String -> Tree String
right str tree = Node str Empty tree

smallTree :: Tree String
smallTree = Node "This" (left "is" (leaf "a")) (left "small" (leaf "tree"))

treeIndent :: String
treeIndent = "|---"

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
