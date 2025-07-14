module Puriq.Lexer 
   ( tokenizar
   )where

import Puriq.Types

-- Funcioón placeholder para el lexer
tokenizar :: String -> [Token]
tokenizar "" = [TokFin]
tokenizar input | all (`elem` "0123456789") (filter (/= ' ') input) &&  not (null (filter (/= ' ') input)) = [TokEntero (read (filter (/= ' ') input)), TokFin] 
                | otherwise = [TokFin]-- Implementacion temporal
