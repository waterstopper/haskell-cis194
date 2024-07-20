data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show

data FailableDouble = Failure
                    | OK Double
  deriving Show

-- Store a person's name, age, and favourite Thing.
data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a