import Data.IORef



questions2 :: [String]
questions2 = []

answers :: [String]
answers = []
 

    --Agregar respuestas a una lista
addAnswers ::  String -> [String]
addAnswers x = answers ++ [x]

    --Agregar las respuestas a las pregutnas 
addAnswersWithQuestions ::[String]-> [[String]]
addAnswersWithQuestions x  = [questions2] ++ [x] 

input ::  IO String
input  = do
    getLine

addQuestion :: String -> [String]->  [String]
addQuestion  x questions=  questions ++ [x]


main :: IO ()
main = do
  
   ref <- newIORef ([] :: [String])

   name <- input 

   --let x = read name :: Integer
   modifyIORef ref (addQuestion "hola")
   --modifyIORef ref (addQuestion "tony")
   readIORef ref >>= print
   
   





