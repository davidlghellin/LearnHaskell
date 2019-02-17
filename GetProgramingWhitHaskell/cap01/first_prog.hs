messyMain :: IO()

messyMain = do
    print "Escriba el correo electronico"
    recipient <- getLine
    print "Titulo del correo"
    title <- getLine
    print "Quien es el autor??"
    autor <- getLine
    print ("Querido " ++ recipient ++ ",\n " ++ 
        "Gracias por comprar " ++ title ++ "\n gracias,\n " ++ autor)
    
    print (createEmail recipient title autor)


-- add
toPart recipient = "Querido " ++ recipient ++ ",\n "
bodyPart bookTitle = "Gracias por comprar " ++ bookTitle ++ "\n "
fromPart autor = "Gracias,\n " ++ autor

createEmail recipient bookTitle autor = toPart recipient ++
                                        bodyPart bookTitle ++
                                        fromPart autor

-- createEmail "Happy Reader" "Learn Haskell" "Will Kurt"