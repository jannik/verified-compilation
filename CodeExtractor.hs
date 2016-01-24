module CodeExtractor where

import Data.List

type Code = [String]

readCodeFile :: String -> IO Code
readCodeFile = fmap lines . readFile

extractDefinition :: String -> Code -> Code
extractDefinition defName (l : ls)
    | defName `isPrefixOf` l = takeWhile (/= "") (l : ls)
    | otherwise = extractDefinition defName ls

texify :: Code -> Code
texify code = ["\\begin{verbatim}"] ++ code ++ ["\\end{verbatim}"]

extract :: String -> String -> String -> IO ()
extract codeFile defName texFile = do
    code <- readCodeFile codeFile
    writeFile texFile . unlines . texify $ extractDefinition defName code

extractAll :: IO ()
extractAll = do
    extract "code/syntax-hoas.elf" "num" "report/code-hoas-exp.tex"
    extract "code/semantics-hoas.elf" "eval/app" "report/code-hoas-eval-app.tex"
    extract "code/trans-hoas-bruijn.elf" "trans-hb/lam" "report/code-trans-hb-lam.tex"
    extract "code/trans-hoas-bruijn.elf" "%block bl-trans-hb" "report/code-trans-hb-block.tex"
    extract "code/trans-hoas-bruijn.elf" "%query 0 *" "report/code-trans-hb-query.tex"
    extract "code/totality-hoas-bruijn.elf" "trans-hb-exists'" "report/code-totality-hb-exists.tex"
    extract "code/totality-hoas-bruijn.elf" "trans-hb-exists'/lam" "report/code-totality-hb-lam.tex"
    extract "code/totality-hoas-bruijn.elf" "trans-hb-exists'/app" "report/code-totality-hb-app.tex"
    extract "code/totality-hoas-bruijn.elf" "move-to-head" "report/code-totality-hb-move-to-head.tex"
    extract "code/soundness-bruijn-stack.elf" "%reduces" "report/code-soundness-bs.tex"
