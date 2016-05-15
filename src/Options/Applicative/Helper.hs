-- |Helper functions to complement "Options.Applicative"
-- 
-- To use, do something like this:
-- 
-- > module Main where
-- >
-- > import Options.Applicative
-- > import Options.Applicative.Helper
-- > 
-- > data ParserResult = Result1
-- >                   | Result2
-- >                   | Result3
-- >   deriving (Show)
-- >
-- > main :: IO ()
-- > main =
-- >   do myResult <- helperExecParser myParser (fpDesc "Demonstration")
-- >      doSomethingWith myResult
-- >
-- > myParser :: Parser ParserResult
-- > myParser =
-- >   subconcat [ command "result1"
-- >                       (infoHelper (pure Result1)
-- >                                   (fpDesc "Result1"))
-- >             , command "result2"
-- >                       (infoHelper (pure Result2)
-- >                                   (fpDesc "Result2"))
-- >             , command "result3"
-- >                       (infoHelper (pure Result3)
-- >                                   (fpDesc "Result3"))
-- >             ]
-- >
-- > doSomethingWith :: ParserResult -> IO ()
-- > doSomethingWith = print
module Options.Applicative.Helper where

import Options.Applicative

-- |Wrapper around 'info' and 'helper'
-- 
-- > info (helper <*> a) ...
--
-- Instead, it's just
--
-- > infoHelper a ...
infoHelper :: Parser a -> InfoMod a -> ParserInfo a
infoHelper a = info (helper <*> a)

-- |Sort of like 'mconcat' for 'Alternative's
-- 
-- Instead of 
-- 
-- > foo <|> bar <|> baz <|> quux <|> ...
-- 
-- Now, it's just
-- 
-- > altconcat [ foo
-- >           , bar
-- >           , baz
-- >           , quux
-- >           ...
-- >           ]
altconcat :: Alternative f => [f a] -> f a
altconcat =
  \case
    [] -> empty
    x : xs -> x <|> altconcat xs

-- |> altconcat . fmap subparser
-- 
-- Instead of
-- 
-- > subparser (command "foo" ...)
-- >   <|> subparser (command "bar" ...)
-- >   <|> subparser (command "baz" ...)
-- >   ...
-- 
-- Instead it's
-- 
-- > subconcat [ command "foo" ...
-- >           , command "bar" ...
-- >           , command "baz" ...
-- >           ...
-- >           ]
subconcat :: [Mod CommandFields a] -> Parser a
subconcat = altconcat . fmap subparser

-- |> mappend fullDesc . progDesc
-- 
-- > mconcat [ fullDesc
-- >         , progDesc "whatever"
-- >         ...
-- >         ]
-- 
-- Now, it's just
-- 
-- > mconcat [ fpDesc "whatever"
-- >         ...
-- >         ]
fpDesc :: String -> InfoMod a
fpDesc = mappend fullDesc . progDesc

-- |Preferences that I like. Using these preferences, your app will 
-- 
-- * disambiguate shortened subcommands
-- * show help whenever someone makes an error.
-- 
-- Note that you should use this in combination with 'infoHelper' for
-- maximum helpfulness.
--
-- > prefs helperPrefsMod
helperPrefs :: ParserPrefs
helperPrefs = prefs helperPrefsMod

-- |The 'PrefsMod' for 'helperPrefs' so that you can add on your own
-- preferences.
-- 
-- > mappend disambiguate showHelpOnError
helperPrefsMod :: PrefsMod
helperPrefsMod = mappend disambiguate showHelpOnError

-- |Wrapper around 'customExecParser', 'helperPrefs', and 'infoHelper'
-- 
-- > helperExecParser a b = customExecParser helperPrefs (infoHelper a b)
helperExecParser :: Parser a -> InfoMod a -> IO a
helperExecParser a b = customExecParser helperPrefs (infoHelper a b)
