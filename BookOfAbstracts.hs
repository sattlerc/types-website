import Control.Arrow ((>>>))
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (pack, unpack)
import System.Directory (createDirectoryIfMissing, copyFile, renameFile)
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>), addExtension, dropExtension, replaceExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(close_fds, cwd, std_err, std_out), callCreateProcess, proc)
import Text.LaTeX (LaTeX, LaTeXT, LaTeXT_)
import Text.LaTeX qualified as LaTeX
import Text.LaTeX.Base.Class qualified as LaTeX
import Text.LaTeX.Base.Pretty qualified as LaTeX
import Text.LaTeX.Base.Syntax qualified as LaTeX

import General
import Parse
import qualified Paths

options :: [(String, Maybe String)] -> String
options = map h >>> intercalate "," where
  h :: (String, Maybe String) -> String
  h (key, val) = case val of
    Nothing -> key
    Just value -> intercalate "=" [key, value]

dir_book_of_abstracts :: FilePath
dir_book_of_abstracts = "book-of-abstracts"

dir_book_of_abstracts_inverse :: FilePath
dir_book_of_abstracts_inverse = ".."

path_book_of_abstracts_core :: FilePath
path_book_of_abstracts_core = dir_book_of_abstracts </> "core.tex"

gen_empty_line :: (Monad m) => LaTeXT_ m
gen_empty_line = LaTeX.fromLaTeX $ LaTeX.TeXRaw "\n"

path_invited_abstracts :: FilePath
path_invited_abstracts = "abstracts-invited"

abstract_invited :: (Monad m) => Invited -> LaTeXT_ m
abstract_invited invited = do
 LaTeX.documentclass ["a4paper"] "easychair"
 LaTeX.usepackage [] "microtype"
 LaTeX.author $ fromString $ invited_speaker invited
 maybeM_ (invited_title invited) $ fromString >>> LaTeX.title
 LaTeX.institute Nothing $ fromString $ invited_affiliation invited
 LaTeX.document do
   LaTeX.maketitle
   maybeM_ (invited_abstract invited) $ fromString

generate_abstracts_invited :: IO ()
generate_abstracts_invited = do
  inviteds <- parse_file_inviteds Paths.inviteds
  createDirectoryIfMissing True path_invited_abstracts
  forM_ (Map.assocs inviteds) $ \(key, invited) ->
    LaTeX.execLaTeXT (abstract_invited invited) >>= LaTeX.renderFile (path_invited_abstracts </> addExtension key "tex")

run_latex :: String -> FilePath -> String -> IO ()
run_latex executable dir basename = replicateM_ 2 $ callCreateProcess $
  (proc executable ["-halt-on-error", basename]) { cwd = Just dir, close_fds = True }

build_abstracts_invited :: IO ()
build_abstracts_invited = do
  inviteds <- parse_file_inviteds Paths.inviteds
  forM_ (Map.keys inviteds) $ \key ->
    run_latex "xelatex" path_invited_abstracts key


gen_section :: (Monad m) => String -> LaTeXT_ m
gen_section title = do
  LaTeX.fromLaTeX $ LaTeX.TeXComm "phantomsection" []
  LaTeX.fromLaTeX $ LaTeX.TeXComm "addcontentsline"
    [ LaTeX.FixArg "toc"
    , LaTeX.FixArg "chapter"
    , LaTeX.FixArg $ fromString title
    ]

gen_index_author :: (Monad m) => Author -> LaTeXT_ m
gen_index_author author = LaTeX.fromLaTeX $ LaTeX.TeXComm "index"
  [LaTeX.FixArg $ fromString $ author_sort_key_string author ++ "@" ++ author_last_first author]

gen_include_pdf :: (Monad m) => String -> FilePath -> LaTeXT_ m
gen_include_pdf title rel_path = LaTeX.fromLaTeX $ LaTeX.TeXComm "includepdf"
    [ LaTeX.OptArg $ LaTeX.raw $ pack $ options
    [ ("pages", Just "-")
    , ("pagecommand*", Just $ unpack $ LaTeX.render page_command)
    ]
  , LaTeX.FixArg $ fromString rel_path
  ] where
  page_command :: LaTeX
  page_command = LaTeX.TeXBraces $ LaTeX.execLaTeXM $ do
    LaTeX.fromLaTeX $ LaTeX.TeXComm "phantomsection" []
    LaTeX.fromLaTeX $ LaTeX.TeXComm "addcontentsline"
      [ LaTeX.FixArg $ fromString "toc"
      , LaTeX.FixArg $ fromString "section"
      , LaTeX.FixArg $ fromString title
      ]
    LaTeX.thispagestyle "empty"

run_pax :: FilePath -> FilePath -> IO ()
run_pax cwd path = callCreateProcess $
  (proc "java" ["-jar", "/usr/share/texmf-dist/scripts/pax/pax.jar", path]) { cwd = Just cwd, close_fds = True }

gen_entry :: (MonadIO m) => [Author] -> String -> FilePath -> LaTeXT_ m
gen_entry authors title path = do
  liftIO $ do
    let target = dir_book_of_abstracts </> rel_path
    copyFile path target
    run_pax dir_book_of_abstracts rel_path
    renameFile (replaceExtension target "pax") (replaceExtension target "newpax")

  forM_ authors gen_index_author
  gen_include_pdf title rel_path
  gen_empty_line
  where
    rel_path :: FilePath
    rel_path = takeFileName path

gen_paper :: (MonadIO m) => Paper -> LaTeXT_ m
gen_paper paper = gen_entry
  (paper_authors paper)
  (paper_title_latex_maybe paper)
  (fromJust $ paper_path paper)

gen_invited :: (MonadIO m) => String -> Invited -> LaTeXT_ m
gen_invited key invited = gen_entry
  [invited_author invited] 
  (invited_title_latex_maybe invited)
  (path_invited_abstracts </> addExtension key "pdf")

gen_book_of_abstracts :: (MonadIO m) => Papers -> Inviteds -> Sessions -> LaTeXT_ m
gen_book_of_abstracts papers inviteds sessions = do
  LaTeX.comment $ pack "Invited talks."
  gen_empty_line
  gen_section "Invited talks"
  forM_ (Map.assocs inviteds) (uncurry gen_invited)
  gen_empty_line

  forM_ (Map.assocs sessions) $ \(session_id, session) -> do
    LaTeX.comment $ pack $ "Session " ++ show_id session_id ++ "."
    gen_empty_line
    gen_section $ session_title session
    forM_ (session_papers session) $ \paper_id -> do
      gen_paper $ papers Map.! paper_id
    gen_empty_line

-- newpax fails on Paper 16.
-- gen_paxnew :: (Monad m) => [FilePath] -> LaTeXT_ m
-- gen_paxnew paths = do
--   LaTeX.documentclass [] "article"
--   LaTeX.fromLaTeX $ LaTeX.TeXComm "directlua" [LaTeX.FixArg $ fromString lua]
--   LaTeX.document $ return () where
--     lua_func_lit :: String -> String -> String
--     lua_func_lit function literal = function ++ "(" ++ show literal ++ ")"
    
--     lua :: String
--     lua = unlines $ ["require(" ++ show "newpax" ++ ")"] ++ map (dropExtension >>> lua_func_lit "newpax.writenewpax") paths

generate :: IO ()
generate = do
  papers <- parse_papers Paths.papers Paths.abstracts
  sessions <- parse_file_sessions Paths.sessions
  inviteds <- parse_file_inviteds Paths.inviteds
  gen <- LaTeX.execLaTeXT $ gen_book_of_abstracts papers inviteds sessions
  createDirectoryIfMissing True dir_book_of_abstracts
  writeFile path_book_of_abstracts_core $ LaTeX.prettyLaTeX gen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      generate_abstracts_invited
      build_abstracts_invited
      generate
    ["generate-abstracts-invited"] -> generate_abstracts_invited
    ["build-abstracts-invited"] -> build_abstracts_invited
    ["generate"] -> generate
    _ -> do
      hPutStrLn stderr "Usage: <executable> (generate-abstracts-invited | build-abstracts-invited | generate)"
