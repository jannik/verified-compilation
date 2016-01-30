{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Twelf where

import Data.List
import Control.Monad.Reader
import Text.Parsec
import System.IO
import System.Process
import Control.Applicative (Applicative, (<$>), (*>), (<*), (<*>))

newtype Twelf a = Twelf (ReaderT (Handle, Handle, Bool) IO a)
    deriving (Functor, Applicative, Monad, MonadReader (Handle, Handle, Bool), MonadIO)

runTwelf :: String -> Bool-> Twelf a -> IO a
runTwelf twelfExe verbose twelf = do
    let (Twelf m) = waitForOK >> twelf
    let creator = (proc twelfExe []) {
            cwd = Just "code",
            std_in = CreatePipe,
            std_out = CreatePipe
        }
    (Just hin, Just hout, _, hproc) <- createProcess creator
    hSetBuffering hin LineBuffering
    hSetBuffering hout LineBuffering
    x <- runReaderT m (hin, hout, verbose)
    hClose hin
    hClose hout
    waitForProcess hproc
    return x

drain :: Handle -> IO ()
drain hndl = do
    r <- hWaitForInput hndl 100
    when r (hGetLine hndl >>= print >> drain hndl)

waitFor :: (String -> Bool) -> Twelf ()
waitFor p = do
    line <- readLineFromTwelf
    when (not $ p line) $ waitFor p

waitForOK :: Twelf ()
waitForOK = waitFor (== "%% OK %%")

readLineFromTwelf :: Twelf String
readLineFromTwelf = do
    (_, hout, verbose) <- ask
    line <- liftIO $ hGetLine hout
    when verbose $ liftIO . putStrLn $ "Twelf: " ++ line
    return line

writeLineToTwelf :: String -> Twelf ()
writeLineToTwelf line = do
    (hin, _, verbose) <- ask
    liftIO $ hPutStrLn hin line
    when verbose $ liftIO . putStrLn $ "Monad: " ++ line

command :: String -> Twelf ()
command cmd = do
    writeLineToTwelf cmd
    waitForOK

loadFile :: String -> Twelf ()
loadFile fileName = command $ "loadFile " ++ fileName

loadSources :: Twelf ()
loadSources = command "Config.load"

readSentence :: Twelf String
readSentence = unlines <$> f
    where
        f = do
            line <- readLineFromTwelf
            case reverse line of
                ('.' : _) -> return $ [init line]
                _ -> (:) line <$> f

query :: TwExp -> Twelf [(String, TwExp)]
query pattern = do
    writeLineToTwelf "readDecl"
    writeLineToTwelf $ "%query 1 1 " ++ show pattern ++ ".%."
    waitFor $ \line -> take 3 line == "---"
    result <- readSentence
    bindings <- case parse pQueryResult "" result of
        Left err -> error $ show err
        Right bindings -> return bindings
    waitForOK
    return bindings

data TwExp = TwName String | TwApp TwExp TwExp | TwLam String TwExp TwExp | TwRaw String

instance Show TwExp where
    show (TwName name) = name
    show (TwApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (TwLam name typ body) = "[" ++ name ++ ":" ++ show typ ++ "]" ++ show body
    show (TwRaw raw) = "(" ++ raw ++ ")"

type P = Parsec String ()

pQueryResult :: P [(String, TwExp)]
pQueryResult = do
    name <- pIdent
    pSymbol '='
    value <- pExp
    (:) (name, value) <$> (pSymbol ';' *> pQueryResult <|> return [])

pExp :: P TwExp
pExp = foldl1 TwApp <$> many1 pSimpleExp

pSimpleExp :: P TwExp
pSimpleExp = (TwName <$> pIdent) <|> (pSymbol '(' *> pExp <* pSymbol ')') <|> pLam

pLam :: P TwExp
pLam = TwLam <$> (pSymbol '[' *> pIdent) <*> (pSymbol ':' *> pExp) <*> (pSymbol ']' *> pExp)

pIdent :: P String
pIdent = many1 (letter <|> digit <|> oneOf "'-/") <* spaces

pSymbol :: Char -> P ()
pSymbol symbol = char symbol >> spaces
