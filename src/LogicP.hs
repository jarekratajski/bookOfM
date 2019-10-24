module LogicP where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Logic
import           Data.Foldable
import           Data.List

--instance Alternative [] where
--    empty = []
--    (<|>) = (++)



type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom", "Tim"]

pcRels :: [(Person, Person)]
pcRels = [("Alejandro","Quique"),("Elena", "Quique"), ("John", "Mary"), ("John", "Tom"), ("Mary","Tim")]

gpgcRels  :: [(Person, Person)]
gpgcRels = do
                (grandp, parent ) <- pcRels
                (parent', grandc) <- pcRels
                guard (parent == parent')
                return (grandp, grandc)


siblingRels :: [(Person, Person)]
siblingRels = do
                  (parent, sibl ) <- pcRels
                  (parent', sibl') <- pcRels
                  guard (parent == parent')
                  guard (sibl /= sibl')
                  guard ( (elemIndex (parent, sibl) pcRels ) < (elemIndex (parent', sibl') pcRels  ))
                  return  (sibl, sibl')

logicPlay :: IO ()
logicPlay = do
                  putStrLn $ show gpgcRels
                  putStrLn $ show siblingRels
                  putStrLn $ show $ observe $ pyts [1,2,3,4,7,8,9,5]
                  putStrLn $ show $ observeMany 6  $ fairTriples  [1,2,3,4,7,8,9,5]
                  putStrLn $ show $ observeMany 2  $ pyts [1 .. ]


list :: [a] -> Logic a
list xs = asum (map return xs)

sums:: [Integer] -> [(Integer, Integer, Integer)]



sums ns = do
            x <- ns
            y <- ns
            z <- ns
            guard (x + y == z)
            return (x, y, z)

fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns =
            list ns >>- \x ->
            list ns >>- \y ->
            list ns >>- \z ->
            return (x, y, z)

pyts :: [Integer] -> Logic (Integer, Integer, Integer)
pyts ns = fairTriples ns >>= \(x,y,z) ->
                guard (x*x + y*y == z*z) >> return (x,y,z)

