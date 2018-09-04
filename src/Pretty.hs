module Pretty where

class Pretty a where
    pretty :: a -> String

pprint :: (Pretty a) => a -> IO ()
pprint = putStrLn . pretty
