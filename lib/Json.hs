module Json (parse) where

import Data.Char (isDigit, isAlphaNum)
import Text.Read


-- type JString = String
-- data JNumber = Integer | Float
-- type JArray = [JsonValue]
-- type JObject = [(String, JsonValue)]

-- data JsonValue =
--   -- JsonInt Integer |
--   -- JsonFloat Float |
--   JsonNumber JNumber |
--   JsonArray JArray |
--   JsonObject JObject |
--   JsonString JString |
--   JsonBool Bool |
--   JsonNull 


data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArrray [JValue]
              deriving (Eq, Ord, Show)


type Parser a b = [a] -> [(b, [a])]

-- number :: String -> Parser String String
-- number = (read p) :: JsonNumber
--   where
--     p (x:xs)
--       | x `elem` ['0'..'9'] = x : p xs
--       | x `elem` ['.'] = x : p xs
--       | otherwise = []

succeed :: b -> Parser a b
succeed val inp = [(val, inp)]

spot :: (Char -> Bool) -> Parser Char String
spot _ [] = []
spot p (x:xs)
  | p x = [([x],xs)]
  | otherwise = []

token :: Char -> Parser Char String
token t = spot (==t)

alt :: Parser a b -> Parser a b -> Parser a b
alt p1 p2 inp = p1 inp ++ p2 inp

infixr 5 >*>
(>*>) :: Parser a b -> Parser a c -> Parser a (b,c)
(>*>) p1 p2 inp = [ ((y, z), rem2) | (y, rem1) <- p1 inp, (z, rem2) <- p2 rem1]

build :: Parser a b -> (b -> c) -> Parser a c
build p f inp = [(f x, rem1) | (x, rem1) <- p inp]

buildFilter:: Parser a b -> (b -> Bool) -> (b -> c) -> Parser a c
buildFilter p keep f inp = [(f x, rem1) | (x, rem1) <- p inp, keep x]

dig :: Parser Char String
dig = spot isDigit

bracket = spot (=='[')

list :: Parser a b -> Parser a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

neList :: Parser a b -> Parser a [b]
neList p x = filter notEmpty (list p x)
  where
    notEmpty ((_:_), _) = True
    notEmpty _ = False

optional :: Parser a b -> Parser a [b]
optional p = (p `build` (:[])) `alt` (succeed [])

strList2num i = (readMaybe (concat i)):: Maybe Double

-- number :: Parser Char [String]
number = build (neList (dig `alt` token '.')) strList2num
jnumber = buildFilter number keep b
  where
    b (Just a) = JNumber a
    b Nothing  = error "Should be filtered and never hit"
    keep (Just _) = True
    keep Nothing  = False
-- jnumber = build number b
--   where
--     b (Just a) = [JNumber a]
--     b Nothing  = []


-- jvalue = jnumber `alt` jstring `alt` jarray `alt` jobject
jvalue = jnumber `alt` jstring `alt` jobject


jarray =   token '['
       >*> list (jvalue >*> (optional $ token ','))
       >*> token ']'

isSimpleChar :: Char -> Bool
isSimpleChar c = isAlphaNum c || c `elem` ['-',':','_']

-- jstring :: String -> [(JValue, String)]
jstring :: Parser Char JValue
jstring =   (token '"'
        >*> list (spot isSimpleChar)
        >*> token '"')
        `build` jstr
          where
            jstr :: (String, ([String], c)) -> JValue
            jstr (_,(l,_)) = JString (concat l)

keyValue = (jstring >*> token ':' >*> jvalue) `build` makeKV
  where
    makeKV (JString k, (_,v)) = (k, v)
    makeKV _ = error "keyValue: Expect the shape to always match, since the parsers matched"


-- ((JString "key1",(":",JNumber 123.0)),"")

jobject =   (token '{'
        >*> list (keyValue >*> (optional $ token ','))
        >*> token '}') `build` makeObj
          where
            makeObj (_,(l,_)) = JObject (map kv l)
            kv (p,_) = p


-- (("key1",JString "ahoj"),[","])
-- ("{",([(("key1",JString "ahoj"),[","]),(("key2",JNumber 123.4),[])],"}"))

parse = "ahoj"



