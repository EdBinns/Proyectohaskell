--import System.Random

listaquemada :: [[[String]]]
listaquemada = [[["e1","p1"],["r1","r2","r3","r4","r5"]],[["e1","p2"],["r1","r2"]],[["e2","p1"],["r1","r2","r3","r4","r5"]],[["e2","p2"],["r1","r2"]]]
    
listaquemadaRespuestas ::[[[String]]]
listaquemadaRespuestas = [[["e1","p1","r1"],["e1","p2","r2"]],[["e2","p1","r1"],["e2","p2","r2"]],[["e1","p1","r1"],["e1","p2","r2"]]]
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
            print("Quiere agregar otra Encuesta?(1 si,0 no)")
            varInput <- input
            let varCondicion = read varInput :: Int
            addFormsNames varCondicion y completeForms
        else return  z

---Función que se encarga de generar preguntas
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
                       print("Quiere agregar otra pregunta? (1 si,0 no)")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion y concatList
                else  do
                       let questionsIO =  addScaleQuestions 0 newQuestion []
                       scaleAnsers <- (questionsIO)
                       let completeForm = addCompleteForm questionAndNameFormList scaleAnsers
                       let concatList = z ++ completeForm
                       print("Quiere agregar otra pregunta? (1 si,0 no)")
                       varInput <- input
                       let varCondicion = read varInput :: Int
                       generateQuestions varCondicion y concatList
        else  return z

---Agregar respuestas para  preguntas  de selección unica 
addUniqueSelection ::Int -> String -> [String]-> IO [String]
addUniqueSelection x y z =  do
     if (x) /= 0
        then do
            let message = "Ingrese  respuesta para la pregunta de " ++ y 
            showMessage message
            newQuestion <- input
            let newQuestionsList = addQuestions newQuestion z
            print("Quiere agregar otra respuesta? (1 si,0 no)")
            varInput <- input
            let varCondicion = read varInput :: Int
            addUniqueSelection varCondicion y newQuestionsList
        else return  z

   
---Agregar respuestas para  preguntas  de tipo escalar 
addScaleQuestions ::Int -> String -> [String]-> IO [String]
addScaleQuestions x y z = do
     if (x) < 5
        then do
            let message = "Ingrese respuesta para la pregunta de " ++ y 
            showMessage message
            newQuestion <- input
            let newQuestionsList = addQuestions newQuestion z
            addScaleQuestions (x + 1) y newQuestionsList
        else return  z

--Verifica que solo vayan las respuestas de la encuesta seleccionada
filterByNameForm :: [[String]] -> String-> [[String]]
filterByNameForm x y= do
    let nameForm = selectByNameForm (x !! 0) y
    if (nameForm) == []
         then do
           []
         else 
           [nameForm] ++ [x !! 1]    
  
--Verifica que preguntas le pertenecen a la encuesta seleccionada
selectByNameForm ::  [String] -> String-> [String]
selectByNameForm x y = 
    if (x !! 0) == y
        then do
            x
        else
            []

--Función que filta la lista de encuestas por la encuesta que desee el usuario
filterForms ::  [[[String]]] -> String->  [[[String]]]
filterForms x f = do
    let y = map(\d ->filterByNameForm d f ) x
    filter (\e -> e/=[]) y
    
--Función que se encarga de que el usuario conteste todas las encuestas
answerForm :: [[[String]]] -> [[String]] ->  IO[[String]]
answerForm x  z = do
    if(x) /= []
        then do
             let question = x !! 0
             let varQuestion = question !! 0
             let varAnswers = question !! 1
             let showOptions = zip [1..(length varAnswers)] varAnswers
             print (varQuestion !! 1)
             print "Ingrese el numero de la respuesta que desea"
             print showOptions
             answers <- input
             let inputAnswers = read answers :: Int
             let realAnswer =  (inputAnswers - 1)
             let selectAnswer = varAnswers !! realAnswer
             let listAnswer = varQuestion ++ [selectAnswer] 
             let complete = z ++ [listAnswer]
             answerForm (tail x)  complete
        else return z
answerAutomatic :: [[[String]]] -> [[String]] ->  IO[[String]]
answerAutomatic x  z = do
    if(x) /= []
        then do
             let question = x !! 0
             let varQuestion = question !! 0
             let varAnswers = question !! 1
             let realAnswer =  0
             let selectAnswer = varAnswers !! realAnswer
             let listAnswer = varQuestion ++ [selectAnswer] 
             let complete = z ++ [listAnswer]
             answerAutomatic (tail x)  complete
        else return z
--Función que le pregunta al usuario que preguntas desea contestar
selectForm ::Int-> [[[String]]] -> [[[String]]] -> IO[[[String]]]
selectForm x y z  = do
    if(x) /= 0
        then do 
           print "Cual encuesta desea contestar?"
           selectFormVar <- input
           print "Desea contestarla de forma automatica o manual?  (1 automatica,0 manual)"
           typeInput <- input
           let typeAnswer = read typeInput :: Int
           let questionsForForms = filterForms y selectFormVar
           if(typeAnswer) == 0
               then do
                    let answersIO = answerForm questionsForForms  []
                    answers <- (answersIO)
                    let fullAnswers = z ++ [answers]
                    print "Desea contestar otra encuesta? (1 si,0 no)"  
                    varInput <- input 
                    let varCondicion = read varInput :: Int
                    selectForm varCondicion y fullAnswers
                else do
                    let answersIO = answerAutomatic questionsForForms  []
                    answers <- (answersIO)
                    let fullAnswers = z ++ [answers]
                    print "Desea contestar otra encuesta? (1 si,0 no)"  
                    varInput <- input 
                    let varCondicion = read varInput :: Int
                    selectForm varCondicion y fullAnswers
        else return z



--Estadistica que nos dice cuantas veces se realizo una encuesta
howManyTimesWasAFormAnswered :: [[[String]]]->String-> Int
howManyTimesWasAFormAnswered x y=do
    let z = map(\d -> verifyAnwers d y) x
    length (filter (\e -> e/=[]) z)

verifyAnwers :: [[String]] -> String-> [[String]]
verifyAnwers x y= do
    let z = (x !! 0) !! 0
    if(z) == y
        then 
             x 
        else []   

--Me genera la cantidad de preguntas que tiene una encuesta
contQuestions :: [[[String]]] -> String -> Int
contQuestions x y= do
    length (filterForms x y)

--Me genera el total de encuestas contestadas
totalAnsweredForms::  [[[String]]]-> Int
totalAnsweredForms x = length(x)  

statisticsMenu ::  [[[String]]] -> [[[String]]] -> Int ->IO()
statisticsMenu x z y= do
    if(y) /= 0
        then do
            print "Elija cual estadistica quiere ver:"
            print "1- Las veces que se contesto una encuesta"
            print "2- El total de preguntas de una encuesta"
            print "3- El total de encuestas realizadas"
            selectStad <- input
            let realSelect = read selectStad ::Int
            opcionsStadistics  realSelect x z
            print "Desea volver a las estadisticas? (1 si, 0 no)"
            varInput <- input
            let varCondicion = read varInput ::Int
            statisticsMenu x z varCondicion 
        else print"Programa finalizado"


opcionsStadistics::Int ->  [[[String]]] -> [[[String]]] -> IO()
opcionsStadistics x y z
       | x == 1 = do 
           print "Ingrese el nombre de la encuesta que desea ver esta estadistica"
           selectForm <- input
           print "El total de veces que una encuesta se contesto es de:"
           print(howManyTimesWasAFormAnswered y selectForm )
       | x == 2 = do
           print "Ingrese el nombre de la encuesta que desea ver esta estadistica"
           selectForm <- input
           print "El total de preguntas de la encuesta son de: "
           print(contQuestions z selectForm )
       | x == 3 = do 
           print "El total de encuestas realizadas es de:  "
           print(totalAnsweredForms y )


menuSystem:: [[[String]]]-> [[[String]]]->IO()
menuSystem  listForms listAnswers = do
     print "Bienvenido al sistema de encuestas"
     print "=================================="
     print "                                  "
     print "Marque que opcion desea"
     print "=================================="
     print "1-Generar encuestas"
     print "2-Contestar encuestas"
     print "3-Ver estadisticas"
     print "4-Salir"
     item <- input
     let itemMenu = read item :: Int
     if(itemMenu) == 1
         then do
             print "=================================="
             let forms = addFormsNames 1 " " listForms
             formsList <- (forms)
             print "=================================="
             menuSystem formsList listAnswers
     else if(itemMenu) == 2
         then do
               let y = selectForm 1 listForms []
               answers <- (y)
               let union = listAnswers ++ answers
               print "=================================="
               menuSystem listForms union
     else if(itemMenu) == 3           
         then do
            statisticsMenu   listAnswers listForms 1
            print "=================================="
            menuSystem listForms listAnswers
     else do
         print("Muchas gracias por su participacion")        
         print "=================================="   

main :: IO ()
main = do
   -- let forms = addFormsNames 1 " " []
    --formsList <- (forms)
    --let y = selectForm 1 listaquemada []
    --answers <- (y)
    --statisticsMenu  answers formsList 1
    --print answers
    menuSystem [] []
    
    