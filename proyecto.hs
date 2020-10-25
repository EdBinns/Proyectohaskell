listaquemada :: [[[String]]]
listaquemada = [[["e1","p1"],["r1","r2","r3","r4","r5"]],[["e1","p2"],["r1","r2"]],[["e2","p1"],["r1","r2","r3","r4","r5"]],[["e2","p2"],["r1","r2"]]]
    
---Función que agrega nuevas respuestas a una lista
addAnswers ::  String -> [String] -> [String]
addAnswers x y  = y ++ [x]


---Función que agrega nuevas preguntas a una lista:
addQuestions :: String -> [String] -> [String]
addQuestions x y = y ++ [x]


---Función que agrega nuevas encuestas a una lista
addForms ::  String -> [String] -> [String]
addForms x y  = y++ [x]


---Función que agrega la encuesta completa a una lista
addCompleteForm :: [String]-> [String] -> [[[String]]]
addCompleteForm x y = [[x]++[y]]
input ::  IO String
input  = do
    getLine
--Función que permite imprimir un mensaje en pantalla
showMessage :: String -> IO()
showMessage  x = print  x

--Función que permite ir añadiendo encuestas
addFormsNames ::Int -> String -> [[[String]]]-> IO [[[String]]]
addFormsNames x y z =  do
     if (x) /= 0
        then do
            let message = "Ingrese el nombre de la encuesta"
            showMessage message
            newName <- input
            print "Seguidamente va a agregar las preguntas"
            let forms = generateQuestions 1 newName [] 
            formsLists <- (forms)
            let completeForms = z ++ formsLists
            print("Quiere agregar otra Encuesta? 1 para si y 0 para no")
            varInput <- input
            let varCondicion = read varInput :: Int
            addFormsNames varCondicion y completeForms
        else return  z
---Agregar preguntas
generateQuestions :: Int -> String ->[[[String]]] -> IO [[[String]]]
generateQuestions x y z= do 
   if (x) /= 0
        then do
            showMessage "Ingrese 1 para que sea de tipo escalar o 2 para que sea de selección unica"
            questionType <- input
            showMessage "Ingrese la pregunta o titulo que desee"
            newQuestion <- input
            let newQuestionList = addQuestions newQuestion []
            let questionAndNameFormList = [y] ++ newQuestionList
            let varType = read questionType :: Int
            if(varType) == 2
                then do
                       let questionsIO =  addUniqueSelection 1 newQuestion []
                       unique <- (questionsIO)
                       let completeForm = addCompleteForm questionAndNameFormList unique
                       let concatList = z ++ completeForm
                       print("Quiere agregar otra pregunta? 1 para si y 0 para no")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion y concatList
                else  do
                       let questionsIO =  addScaleQuestions 0 newQuestion []
                       scaleAnsers <- (questionsIO)
                       let completeForm = addCompleteForm questionAndNameFormList scaleAnsers
                       let concatList = z ++ completeForm
                       print("Quiere agregar otra pregunta? 1 para si y 0 para no")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion y concatList
        else  return z

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

prueba :: [[String]] -> String-> [[String]]
prueba x y= do
    let xd = prueba2 (x !! 0) y
    if (xd) == []
         then do
           []
         else 
           [xd] ++ [x !! 1]    
  

prueba2 ::  [String] -> String-> [String]
prueba2 x y = 
    if (x !! 0) == y
        then do
            x
        else
            []

prueba3 ::  [[[String]]] -> String->  [[[String]]]
prueba3 x f = do
    let y = map(\x ->prueba x f ) listaquemada
    let z =  filter (\e -> e/=[]) y
    z


main :: IO ()
main = do
     --let forms = addFormsNames 1 " " []
    --x <- (forms)
    let f = "e2"
--    let y = map(\x ->prueba x f ) listaquemada
    let y = prueba3 listaquemada f
    print listaquemada
    print y



 
 

    
    