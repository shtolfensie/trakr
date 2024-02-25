module Spec (tests) where

import Json

import Distribution.TestSuite

tests :: IO [Test]
tests = return [ Test number, Test object ]
  where
    number = TestInstance
        { run = do
            let p = parse "132.4"
            return $ if p == Just (JNumber 132.4) then
              Finished $ Pass
            else
              Finished $ Fail "Number parse fail"
              
        , name = "succeeds"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right number
        }
    object = TestInstance
        { run = do
            let p = parse "{\"range\":{\"start\":\"2024-02-25\",\"end\":\"2024-02-25\"},\"total\":37,\"days\":[{\"date\":\"2024-02-25\",\"total\":37,\"projects\":[{\"project\":\"haskell:trakr\",\"duration\":37}]}]}"
            return $ if p == Just (
                JObject [
                ("range",JObject [("start",JString "2024-02-25"),("end",JString "2024-02-25")]),
                ("total",JNumber 37.0),
                ("days",JArrray [
                 JObject [
                 ("date",JString "2024-02-25"),
                 ("total",JNumber 37.0),
                 ("projects",JArrray [
                  JObject [
                  ("project",JString "haskell:trakr"),
                  ("duration",JNumber 37.0)
                  ]])
                 ]])
                ])
            then
              Finished $ Pass
            else
              Finished $ Fail "Invalid trackie output parse"

        , name = "fails"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right object
        }
