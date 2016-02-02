module CodeExtractor where

import Data.List
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import Control.Applicative

import Twelf

type Code = [String]

main :: IO ()
main = do
    extractAll
    createAppendix
    createExample

readCodeFile :: String -> IO Code
readCodeFile = fmap lines . readFile

extractDefinition :: Int -> String -> Code -> Code
extractDefinition n defName (l : ls)
    | defName `isPrefixOf` l = takeLines n (l : ls)
    | otherwise = extractDefinition n defName ls

takeLines :: Int -> Code -> Code
takeLines _ [] = []
takeLines 0 _ = []
takeLines 1 ("" : ls) = []
takeLines n ("" : ls) = "" : takeLines (n - 1) ls
takeLines n (l : ls) = l : takeLines n ls

texify :: Code -> Code
texify code = ["\\begin{verbatim}"] ++ code ++ ["\\end{verbatim}"]

extract :: Int -> String -> String -> String -> IO ()
extract n codeFile defName texFile = do
    code <- readCodeFile codeFile
    writeFile texFile . unlines . texify $ extractDefinition n defName code

extractAll :: IO ()
extractAll = do
    extract 2 "code/syntax-hoas.elf" "exp" "report/code-hoas-exp.tex"
    extract 1 "code/semantics-hoas.elf" "eval" "report/code-hoas-eval.tex"
    extract 1 "code/semantics-hoas.elf" "eval/app" "report/code-hoas-eval-app.tex"
    extract 1 "code/trans-hoas-bruijn.elf" "trans-hb/lam" "report/code-trans-hb-lam.tex"
    extract 1 "code/trans-hoas-bruijn.elf" "%block bl-trans-hb" "report/code-trans-hb-block.tex"
    extract 1 "code/trans-hoas-bruijn.elf" "%query 0 *" "report/code-trans-hb-query.tex"
    extract 1 "code/totality-hoas-bruijn.elf" "bound-to-free" "report/code-totality-bound-to-free.tex"
    extract 1 "code/totality-hoas-bruijn.elf" "trans-hb-exists'" "report/code-totality-hb-exists.tex"
    extract 1 "code/totality-hoas-bruijn.elf" "trans-hb-exists'/lam" "report/code-totality-hb-lam.tex"
    extract 1 "code/totality-hoas-bruijn.elf" "trans-hb-exists'/app" "report/code-totality-hb-app.tex"
    extract 1 "code/soundness-bruijn-stack.elf" "%reduces DP' < DP (soundness-bs'" "report/code-soundness-bs.tex"

appendixCode :: [(String, [String])] -> String
appendixCode = unlines . concat . map f
    where
        f (fileName, contents) = ["\\subsubsection{" ++ fileName ++ "}", "\\begin{Verbatim}[fontsize=\\scriptsize]"] ++ contents ++ ["\\end{Verbatim}"]

createAppendix :: IO ()
createAppendix = do
    fileNames <- filter ((== "fle.") . take 4 . reverse) . lines <$> readFile "code/sources.cfg"
    tuples <- sequence [readFile ("code/" ++ fileName) >>= \contents -> return (fileName, lines contents) | fileName <- fileNames]
    writeFile "report/code-appendix.tex" $ appendixCode tuples

twelfPath = "/home/mkm/twelfbin/twelf/bin/twelf-server"
twelfPath' = "C:\\Program Files\\Twelf\\bin\\twelf-server.bat"
twelfPath'' = "C:\\Program Files (x86)\\Twelf\\bin\\twelf-server.bat"

createExample :: IO ()
createExample = runTwelf twelfPath False $ do
    loadSources
    [("E", hexp), ("B", bexp)] <- query (TwRaw "trans-hb-exists test5 (_ : trans-hb env/nil B E)")
    [("P", sprog)] <- query (TwName "trans-bs" `TwApp` bexp `TwApp` TwName "P")
    [("L", start), ("Q", mprog)] <- query (TwName "trans-sm" `TwApp` sprog `TwApp` TwName "Q" `TwApp` TwName "L")
    liftIO . writeFile "report/code-example-hoas.tex" . texifyHExp 1 . mkHExp $ hexp
    liftIO . writeFile "report/code-example-bruijn.tex" . texifyBExp 1 . mkBExp $ bexp
    liftIO . writeFile "report/code-example-stack.tex" . texifySProg . mkSProg $ sprog
    liftIO . writeFile "report/code-example-machine.tex" . texifyMProg . mkMProg $ mprog

mkNat :: TwExp -> Integer
mkNat (TwName "z") = 0
mkNat (TwApp (TwName "s") e) = 1 + mkNat e
mkNat (TwName [digit]) | ord '0' <= ord digit && ord digit <= ord '9' = read [digit]

data HExp = HNum Integer | HVar String | HLam String HExp | HApp HExp HExp | HSuc HExp
    deriving (Show)

mkHExp :: TwExp -> HExp
mkHExp (TwName "num" `TwApp` e) = HNum $ mkNat e
mkHExp (TwName name) = HVar (filter (not . isDigit) name)
mkHExp (TwName "lam" `TwApp` TwLam name _ e) = HLam (filter (not . isDigit) name) $ mkHExp e
mkHExp (TwName "app" `TwApp` e1 `TwApp` e2) = HApp (mkHExp e1) (mkHExp e2)
mkHExp (TwName "suc" `TwApp` e) = HSuc $ mkHExp e

texifyHExp :: Int -> HExp -> String
texifyHExp _ (HNum n) = "\\n{" ++ show n ++ "}"
texifyHExp _ (HVar name) = name
texifyHExp 1 (HLam name e) = "\\lam{" ++ name ++ "}{" ++ texifyHExp 1 e ++ "}"
texifyHExp _ (HLam name e) = "(\\lam{" ++ name ++ "}{" ++ texifyHExp 1 e ++ "})"
texifyHExp 3 (HApp e1 e2) = "(\\app{" ++ texifyHExp 2 e1 ++ "}{" ++ texifyHExp 3 e2 ++ "})"
texifyHExp _ (HApp e1 e2) = "\\app{" ++ texifyHExp 2 e1 ++ "}{" ++ texifyHExp 3 e2 ++ "}"
texifyHExp 3 (HSuc e) = "(\\hsuc{" ++ texifyHExp 3 e ++ "})"
texifyHExp _ (HSuc e) = "\\hsuc{" ++ texifyHExp 3 e ++ "}"

data BExp = BNum Integer | BVar Integer | BLam BExp | BApp BExp BExp | BSuc BExp
    deriving (Show)

mkBExp :: TwExp -> BExp
mkBExp (TwName "bnum" `TwApp` e) = BNum $ mkNat e
mkBExp (TwName "bvar" `TwApp` e) = BVar $ mkNat e
mkBExp (TwName "blam" `TwApp` e) = BLam $ mkBExp e
mkBExp (TwName "bapp" `TwApp` e1 `TwApp` e2) = BApp (mkBExp e1) (mkBExp e2)
mkBExp (TwName "bsuc" `TwApp` e) = BSuc $ mkBExp e

texifyBExp :: Int -> BExp -> String
texifyBExp _ (BNum n) = "\\n{" ++ show n ++ "}"
texifyBExp _ (BVar i) = show i
texifyBExp 1 (BLam e) = "\\blam{" ++ texifyBExp 1 e ++ "}"
texifyBExp _ (BLam e) = "(\\blam{" ++ texifyBExp 1 e ++ "})"
texifyBExp 3 (BApp e1 e2) = "(\\bapp{" ++ texifyBExp 2 e1 ++ "}{" ++ texifyBExp 3 e2 ++ "})"
texifyBExp _ (BApp e1 e2) = "\\bapp{" ++ texifyBExp 2 e1 ++ "}{" ++ texifyBExp 3 e2 ++ "}"
texifyBExp 3 (BSuc e) = "(\\bsuc{" ++ texifyBExp 3 e ++ "})"
texifyBExp _ (BSuc e) = "\\bsuc{" ++ texifyBExp 3 e ++ "}"

data SExp = SNum Integer | SVar Integer | SLam [SExp] | SApp | SSuc
    deriving (Show)

type SProg = [SExp]

mkSExp :: TwExp -> SExp
mkSExp (TwName "snum" `TwApp` e) = SNum $ mkNat e
mkSExp (TwName "svar" `TwApp` e) = SVar $ mkNat e
mkSExp (TwName "slam" `TwApp` e) = SLam $ mkSProg e
mkSExp (TwName "sapp") = SApp
mkSExp (TwName "ssuc") = SSuc

mkSProg :: TwExp -> SProg
mkSProg (TwName "sprog/nil") = []
mkSProg (TwName "sprog/cons" `TwApp` e `TwApp` es) = mkSExp e : mkSProg es

texifySExp' :: SExp -> [String]
texifySExp' (SNum n) = ["\\snum{" ++ show n ++ "}\\\\"]
texifySExp' (SVar i) = ["\\svar{" ++ show i ++ "}\\\\"]
texifySExp' (SLam es) = ["\\slamkeyword\\\\"] ++ indent (texifySProg' es)
texifySExp' SApp = ["\\sapp\\\\"]
texifySExp' SSuc = ["\\ssuc\\\\"]

indent :: [String] -> [String]
indent = map ("\\cdot \\;\\; " ++)

texifySProg' :: SProg -> [String]
texifySProg' = concatMap texifySExp'

texifySProg :: SProg -> String
texifySProg = unlines . map ('&' :) . texifySProg'


data MExp = MPushNum Integer | MPushVar Integer | MPushClos Integer | MCall | MInc | MRet | MHalt | MDummy
    deriving (Show)

type MProg = [MExp]

mkMExp :: TwExp -> MExp
mkMExp (TwName "mpushnum" `TwApp` e) = MPushNum $ mkNat e
mkMExp (TwName "mpushvar" `TwApp` e) = MPushVar $ mkNat e
mkMExp (TwName "mpushclos" `TwApp` e) = MPushClos $ mkNat e
mkMExp (TwName "mcall") = MCall
mkMExp (TwName "minc") = MInc
mkMExp (TwName "mret") = MRet
mkMExp (TwName "mhalt") = MHalt

mkMProg :: TwExp -> MProg
mkMProg (TwLam nil _ e) = f e
    where
        f (TwName nil') | nil' == nil = []
        f (TwName "mprog'/cons" `TwApp` e `TwApp` es) = mkMExp e : f es

texifyMExp :: MExp -> String
texifyMExp (MPushNum n) = "\\mpushnum{" ++ show n ++ "}"
texifyMExp (MPushVar i) = "\\mpushvar{" ++ show i ++ "}"
texifyMExp (MPushClos ell) = "\\mpushclos{" ++ show ell ++ "}"
texifyMExp MCall = "\\mcall"
texifyMExp MInc = "\\minc"
texifyMExp MRet = "\\mret"
texifyMExp MHalt = "\\mhalt"
texifyMExp MDummy = ""

texifyMProg :: MProg -> String
texifyMProg es = unlines ["\\text{" ++ show i ++ ":}& \\quad" ++ texifyMExp e1 ++ "&&\\text{" ++ show (i + es1Len) ++ ":}\\quad " ++ texifyMExp e2 ++ "\\\\" | (e1, e2, i) <- zip3 es1 es2 [0 ..]]
    where
        es1Len = (length es + 1) `div` 2
        es1 = take es1Len es
        es2 = drop es1Len es ++ (if odd (length es) then [MDummy] else [])
