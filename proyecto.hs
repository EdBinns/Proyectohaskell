

addAnswers ::  String -> [String] -> [String]
addAnswers x y  = y ++ [x]

addQuestions :: [String] -> [String] -> [[String]]
addQuestions x y = [y] ++ [x]

addForms ::  String -> [String] -> [String]
addForms x y  = y++ [x]

input ::  IO String
input  = do
    getLine

showMessage :: String -> IO()
showMessage  x = print  x


---Agregar Encuestas
menuLoop :: Int -> [String] -> IO [String]
menuLoop x y = do 
   if (x) /= 0
        then do
            showMessage "Cual es el nombre de la encuesta?"
            newForm <- input
            let newFormList = addForms newForm y
            print("Quiere agregar otra encuesta? 1 para si y 0 para no")
            varInput <- input
            let varCondicion = read varInput :: Int
            menuLoop varCondicion newFormList
        else  return y
---Agregar Preguntas


getForms :: [String] -> [String]
getForms x = [] ++ x 

main :: IO ()
main = do

    
    let forms = menuLoop 1 []
    x <- (forms)
    print x
    
    