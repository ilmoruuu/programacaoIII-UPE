module ExercíciosP3.Atividade06.AtividadeSeis where
import Data.ByteString.Char8 (readInt)

--Questao 01
meuPutStr :: String -> IO ()
meuPutStr s = sequence_ [putChar x | x <- s]

--Questao 02
