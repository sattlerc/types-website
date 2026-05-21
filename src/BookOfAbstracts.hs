import Control.Arrow ((>>>), (&&&))
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Char (isSpace, toUpper)
import Data.List (intercalate, dropWhileEnd)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text (pack, unpack)
import System.Directory (createDirectoryIfMissing, copyFile, renameFile)
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>), addExtension, dropExtension, replaceExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(close_fds, cwd, std_err, std_out), callCreateProcess, proc, readProcess)
import Text.LaTeX (LaTeX, LaTeXM, LaTeXT, LaTeXT_)
import Text.LaTeX qualified as LaTeX
import Text.LaTeX.Base.Class qualified as LaTeX
import Text.LaTeX.Base.Pretty qualified as LaTeX
import Text.LaTeX.Base.Syntax qualified as LaTeX

import General
import Parse
import qualified Paths

latex_strict :: (Monad m) => LaTeXM () -> LaTeXT_ m
latex_strict = LaTeX.execLaTeXM >>> LaTeX.render >>> LaTeX.raw

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

path_book_of_abstracts_committees :: FilePath
path_book_of_abstracts_committees = dir_book_of_abstracts </> "committees.tex"

file_newpax_generator :: FilePath
file_newpax_generator = "newpax_generator.tex"

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
 maybeM_ (person_affiliation $ invited_person invited) $ fromString >>> LaTeX.institute Nothing
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


type Reference = (LaTeX, String)

ref_from_string :: String -> Reference
ref_from_string s = (fromString s, s)

ref_from_title_latex :: (String, Maybe String) -> Reference
ref_from_title_latex (title, latex_maybe) = (latex, title) where
  latex = case latex_maybe of
    Nothing -> LaTeX.fromString title
    Just s -> LaTeX.raw $ pack s


gen_tex_or_pdf :: (Monad m) => Reference -> LaTeXT_ m
gen_tex_or_pdf (tex, pdf) = do
  LaTeX.fromLaTeX $ LaTeX.TeXComm "texorpdfstring"
    [ LaTeX.FixArg tex
    , LaTeX.FixArg $ fromString pdf
    ]

gen_toc_entry :: (Monad m) => String -> Reference -> LaTeXT_ m
gen_toc_entry heading title = do
  LaTeX.fromLaTeX $ LaTeX.TeXComm "phantomsection" []
  LaTeX.fromLaTeX $ LaTeX.TeXComm "addcontentsline"
    [ LaTeX.FixArg "toc"
    , LaTeX.FixArg $ fromString heading
    , LaTeX.FixArg $ LaTeX.execLaTeXM $ gen_tex_or_pdf title
    ]

gen_toc_chapter :: (Monad m) => Reference -> LaTeXT_ m
gen_toc_chapter = gen_toc_entry "chapter"

gen_toc_section :: (Monad m) => Reference -> LaTeXT_ m
gen_toc_section = gen_toc_entry "section"

gen_index_author :: (Monad m) => Person -> LaTeXT_ m
gen_index_author author = LaTeX.fromLaTeX $ LaTeX.TeXComm "index"
  [LaTeX.FixArg $ fromString $ name_sort_key_string (person_name author) ++ "@" ++ format_name_last_first (person_name author)]

gen_include_pdf :: (Monad m) => Reference -> FilePath -> LaTeXT_ m
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
    gen_toc_section title
    LaTeX.thispagestyle "empty"

run_pax :: FilePath -> FilePath -> IO ()
run_pax cwd path = do
  texroot <- trim <$> readProcess "kpsewhich" ["-var-value", "TEXMFMAIN"] ""
  callCreateProcess $
    (proc "java" ["-jar", texroot ++ "/scripts/pax/pax.jar", path]) { cwd = Just cwd, close_fds = True }
  where
    trim = dropWhileEnd isSpace

-- newpax v0.57 fails on Paper 16.
-- Reported and fixed in v0.58.
gen_newpax :: (Monad m) => [FilePath] -> LaTeXT_ m
gen_newpax paths = do
  LaTeX.documentclass [] "article"
  LaTeX.fromLaTeX $ LaTeX.TeXComm "directlua" [LaTeX.FixArg $ fromString lua]
  LaTeX.document $ return () where
    lua_func_lit :: String -> String -> String
    lua_func_lit function literal = function ++ "(" ++ show literal ++ ")"

    lua :: String
    lua = unlines $ ["require(" ++ show "newpax" ++ ")"] ++ map (dropExtension >>> lua_func_lit "newpax.writenewpax") paths

run_newpax :: [FilePath] -> IO ()
run_newpax paths = do
  LaTeX.execLaTeXT (gen_newpax paths) >>= LaTeX.renderFile (dir_book_of_abstracts </> file_newpax_generator)
  run_latex "lualatex" dir_book_of_abstracts file_newpax_generator

gen_entry :: (MonadIO m) => [Person] -> (String, Maybe String) -> FilePath -> WriterT [FilePath] (LaTeXT m) ()
gen_entry authors title_data path = do
  tell [rel_path]
  liftIO $ do
    let target = dir_book_of_abstracts </> rel_path
    copyFile path target
    -- run_pax dir_book_of_abstracts rel_path
    -- renameFile (replaceExtension target "pax") (replaceExtension target "newpax")

  lift $ do
    forM_ authors gen_index_author
    gen_include_pdf (ref_from_title_latex title_data) rel_path
    gen_empty_line
  where
    rel_path :: FilePath
    rel_path = takeFileName path

gen_paper :: (MonadIO m) => Paper -> WriterT [FilePath] (LaTeXT m) ()
gen_paper paper = gen_entry
  (paper_authors paper)
  ((paper_title &&& paper_title_latex) $ paper)
  (fromJust $ paper_path paper)

gen_invited :: (MonadIO m) => String -> Invited -> WriterT [FilePath] (LaTeXT m) ()
gen_invited key invited = gen_entry
  [invited_person invited]
  (((invited_title >>> fromJust) &&& invited_title_latex) $ invited)
  (path_invited_abstracts </> addExtension key "pdf")

gen_book_of_abstracts :: (MonadIO m) => Papers -> Inviteds -> Sessions -> Schedule -> WriterT [FilePath] (LaTeXT m) ()
gen_book_of_abstracts papers inviteds sessions schedule = do
  lift $ do
    LaTeX.comment $ pack "Invited talks."
    gen_empty_line
    gen_toc_chapter $ ref_from_string "Invited talks"
  forM_ (invited_key_by_schedule schedule) $ \key ->
    gen_invited key (inviteds Map.! key)
  lift gen_empty_line

  forM_ (Map.assocs sessions) $ \(session_id, session) -> do
    lift $ do
      LaTeX.comment $ pack $ "Session " ++ show_id session_id ++ "."
      gen_empty_line
      gen_toc_chapter $ ref_from_string $ update_head toUpper $ session_title session
    forM_ (session_papers session) $ \paper_id -> do
      gen_paper $ papers Map.! paper_id
    lift gen_empty_line

gen_committee :: forall m. (Monad m) => String -> [Person] -> LaTeXT_ m
gen_committee title persons = do
  LaTeX.section' $ fromString title
  LaTeX.tabular Nothing [LaTeX.Separator mempty, LaTeX.LeftColumn, LaTeX.LeftColumn] $ forM_ persons h where
    h :: Person -> LaTeXT_ m
    h person = left LaTeX.& right where
      left :: LaTeXT_ m
      left = do
        fromString (format_name $ person_name person)
        -- Workaround for bug in HaTeX 3.23.0.1:
        -- Without latex_strict, this introduces whitespace before the closing parenthesis.
        maybeM_ (person_role person) $ \role -> latex_strict $ do
          fromString " ("
          LaTeX.emph (fromString role)
          fromString ")"

      right :: LaTeXT_ m
      right = do
        maybeM_ (person_affiliation person) $ fromString
        LaTeX.lnbk

gen_local_organizers :: (Monad m) => [Person] -> LaTeXT_ m
gen_local_organizers persons = do
  LaTeX.section' "Local organizers"
  fromString $ intercalate ", " $ map (person_name >>> format_name) persons

gen_committees :: (Monad m) => [Person] -> [Person] -> [Person] -> LaTeXT_ m
gen_committees organizing_committee program_committee steering_committee = do
  gen_committee "Programme Committee" program_committee
  gen_committee "Steering Committee" steering_committee
  gen_local_organizers organizing_committee

generate :: IO ()
generate = do
  organizing_committee <- parse_file_committee Paths.organizing_committee
  program_committee <- parse_file_committee Paths.program_committee
  steering_committee <- parse_file_committee Paths.steering_committee
  writeFile path_book_of_abstracts_committees $ LaTeX.prettyLaTeX $ LaTeX.execLaTeXM $ gen_committees organizing_committee program_committee steering_committee

  papers <- parse_papers Paths.papers (Just Paths.abstracts) Nothing
  sessions <- parse_file_sessions Paths.sessions
  inviteds <- parse_file_inviteds Paths.inviteds
  schedule <- parse_file_schedule Paths.schedule
  createDirectoryIfMissing True dir_book_of_abstracts
  (((), abstracts), latex) <- LaTeX.runLaTeXT $ runWriterT $ gen_book_of_abstracts papers inviteds sessions schedule
  run_newpax abstracts
  writeFile path_book_of_abstracts_core $ LaTeX.prettyLaTeX $ latex

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
