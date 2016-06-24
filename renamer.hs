import System.Directory (renameFile, getDirectoryContents, doesFileExist)
import System.IO (hPutStrLn,stderr)
import System.IO.Error(userError, ioeGetErrorString)
import System.Environment (getArgs)
import System.FilePath.Posix (replaceBaseName, (</>))
import Control.Exception (try)
import Control.Monad (filterM)

help :: String
help = "Syntax: program-name directory\n\
\Semantics: changes every filename in directory\n\
\to `number ++ \".\" ++ ext` where number starts\n\
\at 0 increasing 1 and ext is the extension of\n\
\each filename. In case of error renaming a file it\n\
\will report its name to stderr. Renaming a file\n\
\may fail when System.Directory.renameFile fails\n\
\plus when exist a file with the new name."

main :: IO ()
main = do
  ar <- getArgs
  if length ar == 1 then do
      ls <- ((renameBaseName $ map show [0..]). head) ar
      _ <- mapM (\x -> case x of
           Left a -> hPutStrLn stderr $ ioeGetErrorString a
           Right _ -> return ())
        ls
      return ()
    else hPutStrLn stderr help  

renameBaseName :: [String] -> FilePath -> IO [Either IOError ()]
renameBaseName l d = do
  ls <- listFiles d
  mapM (\(a,b) -> do
           let n = replaceBaseName a b
           e <- doesFileExist n
           if not e
             then try $ renameFile a n
             else return $ Left $ userError a)
    $ zip ls l

listFiles :: FilePath -> IO [FilePath]
listFiles d = do
  l <- getDirectoryContents d
  filterM doesFileExist $ map (d </>) l
