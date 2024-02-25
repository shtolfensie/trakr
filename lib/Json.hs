module Json (parse, JValue(..)) where

import Data.Char (isDigit, isAlphaNum)
import Text.Read ( readMaybe )

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArrray [JValue]
              deriving (Eq, Ord, Show)


type Parser a b = [a] -> [(b, [a])]

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

list :: Parser a b -> Parser a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

neList :: Parser a b -> Parser a [b]
neList p x = filter notEmpty (list p x)
  where
    notEmpty ((_:_), _) = True
    notEmpty _ = False

optional :: Parser a b -> Parser a [b]
optional p = (p `build` (:[])) `alt` (succeed [])

strList2num :: Foldable t => t [Char] -> Maybe Double
strList2num i = (readMaybe (concat i)):: Maybe Double

lastParse :: [a] -> [a]
lastParse [] = []
lastParse (x:[]) = [x]
lastParse (_:xs) = lastParse xs

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:[]) = Just x
lastMaybe (_:xs) = lastMaybe xs

number :: Parser Char (Maybe Double)
number = build (neList (dig `alt` token '.')) strList2num

jnumber :: Parser Char JValue
jnumber inp = lastParse $ buildFilter number keep b inp
  where
    b (Just a) = JNumber a
    b Nothing  = error "Should be filtered and never hit"
    keep (Just _) = True
    keep Nothing  = False


jvalue :: Parser Char JValue
jvalue = jnumber `alt` jstring `alt` jnull `alt` jbool `alt` jarray `alt` jobject


jarray :: Parser Char JValue
jarray =   (token '['
       >*> list (jvalue >*> (optional $ token ','))
       >*> token ']')
      `build` jarr
        where
          jarr (_,(l,_)) = JArrray (map jitem l)
          jitem (i,_) = i

isSimpleChar :: Char -> Bool
isSimpleChar c = isAlphaNum c || c `elem` ['-',':','_']

jstring :: Parser Char JValue
jstring =   (token '"'
        >*> list (spot isSimpleChar)
        >*> token '"')
        `build` jstr
          where
            jstr :: (String, ([String], c)) -> JValue
            jstr (_,(l,_)) = JString (concat l)

keyValue :: Parser Char (String, JValue)
keyValue = (jstring >*> token ':' >*> jvalue) `build` makeKV
  where
    makeKV (JString k, (_,v)) = (k, v)
    makeKV _ = error "keyValue: Expect the shape to always match, since the parsers matched"


jobject :: Parser Char JValue
jobject =   (token '{'
        >*> list (keyValue >*> (optional $ token ','))
        >*> token '}') `build` makeObj
          where
            makeObj (_,(l,_)) = JObject (map kv l)
            kv (p,_) = p


jnull :: Parser Char JValue
jnull =  (token 'n'
      >*> token 'u'
      >*> token 'l'
      >*> token 'l')
      `build` jn
        where
          jn (_,(_,(_,_))) = JNull


jbool :: Parser Char JValue
jbool = (jtrue `alt` jfalse) `build` mkBool
  where
    mkBool "true" = JBool True
    mkBool "false" = JBool False
    mkBool _ = error "Should only be called on a parsed jbool"

jtrue :: Parser Char [Char]
jtrue =  (token 't'
      >*> token 'r'
      >*> token 'u'
      >*> token 'e')
      `build` cat
        where
          cat (t,(r,(u,e))) = concat [t,r,u,e]

jfalse :: Parser Char [Char]
jfalse =  (token 'f'
       >*> token 'a'
       >*> token 'l'
       >*> token 's'
       >*> token 'e')
      `build` cat
        where
          cat (f,(a,(l,(s,e)))) = concat [f,a,l,s,e]

parse :: String -> Maybe JValue
parse = val . lastMaybe . jvalue
  where
    val (Just (v,_)) = Just v
    val Nothing = Nothing



-- "{\"range\":{\"start\":\"2024-02-25\",\"end\":\"2024-02-25\"},\"total\":37,\"days\":[{\"date\":\"2024-02-25\",\"total\":37,\"projects\":[{\"project\":\"haskell:trakr\",\"duration\":37}]}]}"
-- JObject [
--   ("range",JObject [("start",JString "2024-02-25"),("end",JString "2024-02-25")]),
--   ("total",JNumber 37.0),
--   ("days",JArrray [
--    JObject [
--     ("date",JString "2024-02-25"),
--     ("total",JNumber 37.0),
--     ("projects",JArrray [
--      JObject [
--       ("project",JString "haskell:trakr"),
--       ("duration",JNumber 37.0)
--      ]])
--    ]])
-- ]
