import ST
import Predicate

type Messasge = Int

validMessage :: Predicate Int
validMessage = posNum

protocol :: ST Int
protocol = Send validMessage .- ("another", protocol) <|> ("finish", End)

protocol2 :: ST Int
protocol2 = Send (posNum :: Predicate Int) .- protocol

protocol3 :: ST Int
protocol3 = Send posNum $ \n -> foldr (.-) End (replicate n (Send validMessage))
