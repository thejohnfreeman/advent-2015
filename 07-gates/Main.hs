import Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.Map as M
import Text.Parsec hiding (State)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser $ emptyDef
  { P.reservedNames = ["AND", "OR", "RSHIFT", "LSHIFT", "NOT"]
  , P.reservedOpNames = ["->"]
  }

identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
natural = fromInteger <$> P.natural lexer

data Expr
  = LShift Expr !Word16
  | RShift Expr !Word16
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Value !Word16
  | Name !String
  deriving (Show, Eq)

type Store = M.Map String Expr

wire :: Parser Expr
wire = Value <$> natural
   <|> Name <$> identifier

expr :: Parser Expr
expr = foldr ((<|>) . try) (Value <$> natural)
       [ LShift <$> wire <* reserved "LSHIFT" <*> natural
       , RShift <$> wire <* reserved "RSHIFT" <*> natural
       , And <$> wire <* reserved "AND" <*> wire
       , Or <$> wire <* reserved "OR" <*> wire
       , Not <$ reserved "NOT" <*> wire
       , Name <$> identifier
       ]

store :: Parser Store
store = M.fromList <$> entry `manyTill` eof
  where entry = flip (,) <$> expr <* reservedOp "->" <*> identifier

eval :: Expr -> State Store Word16
eval (Value v) = return v
eval (Name n) = do
  s <- get
  let e = M.findWithDefault (error $ "missing: " ++ n) n s
  v <- eval e
  put $ M.insert n (Value v) s
  return v
eval (Not e) = complement <$> eval e
eval (Or a b) = liftM2 (.|.) (eval a) (eval b)
eval (And a b) = liftM2 (.&.) (eval a) (eval b)
eval (LShift e i) = flip shiftL (fromIntegral i) <$> eval e
eval (RShift e i) = flip shiftR (fromIntegral i) <$> eval e

main :: IO ()
-- main = getContents >>= putStrLn . either show (show . flip eval (Name "a")) . parse store "<stdin>"
main = getContents >>= putStrLn . either show (show . evalState (eval $ Name "gg")) . parse store "<stdin>"
