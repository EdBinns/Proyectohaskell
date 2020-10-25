

addAnswers ::  String -> [String] -> [String]
addAnswers x y  = y ++ [x]

addQuestions :: String -> [String] -> [String]
addQuestions x y = y ++ [x]

addForms ::  String -> [String] -> [String]
addForms x y  = y++ [x]

addCompleteForm :: [String]-> [String] -> [[[String]]]
addCompleteForm x y = [[x]++[y]]
input ::  IO String
input  = do
    getLine

showMessage :: String -> IO()
showMessage  x = print  x
---Agregar Encuestas
generateQuestions :: Int -> [[[String]]] -> IO [[[String]]]
generateQuestions x y = do 
   if (x) /= 0
        then do
            showMessage "Ingrese la pregunta o titulo que desee"
            newQuestion <- input
            let newQuestionList = addForms newQuestion []
            showMessage "Ingrese 1 para que sea de tipo escalar o 2 para que sea de selección unica"
            questionType <- input 
            let varType = read questionType :: Int
            if(varType) == 2
                then do
                       let questionsIO =  addUniqueSelection 1 newQuestion []
                       unique <- (questionsIO)
                       let completeForm = addCompleteForm newQuestionList unique
                       let concatList = y ++ completeForm
                       print("Quiere agregar otra pregunta? 1 para si y 0 para no")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion concatList
                else  do
                       let questionsIO =  addScaleQuestions 0 newQuestion []
                       scaleAnsers <- (questionsIO)
                       let completeForm = addCompleteForm newQuestionList scaleAnsers
                       let concatList = y ++ completeForm
                       print("Quiere agregar otra pregunta? 1 para si y 0 para no")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion concatList
        else  return y

---Agregar respuestas para  preguntas  de selección unica 
addUniqueSelection ::Int -> String -> [String]-> IO [String]
addUniqueSelection x y z =  do
     if (x) /= 0
        then do
            let message = "Ingrese su primer respuesta para la pregunta de " ++ y 
            showMessage message
            newQuestion <- input
            let newQuestionsList = addQuestions newQuestion z
            print("Quiere agregar otra respuesta? 1 para si y 0 para no")
            varInput <- input
            let varCondicion = read varInput :: Int
            addUniqueSelection varCondicion y newQuestionsList
        else return  z

   
---Agregar respuestas para  preguntas  de tipo escalar 
addScaleQuestions ::Int -> String -> [String]-> IO [String]
addScaleQuestions x y z = do
     if (x) < 5
        then do
            let message = "Ingrese su primer respuesta para la pregunta de " ++ y 
            showMessage message
            newQuestion <- input
            let newQuestionsList = addQuestions newQuestion z
            addScaleQuestions (x + 1) y newQuestionsList
        else return  z

main :: IO ()
main = do

    let forms = generateQuestions 1 []
    x <- (forms)
    print x


 
 

    
    