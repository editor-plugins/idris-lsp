module IdrisLsp.Command where

import Data.List()
import IdrisLsp.IdeMode (SExp(..), parseSExp, convSExp)
import System.IO (Handle, hGetChar, hPutStrLn, hSetBuffering, BufferMode(..), hClose)
import Control.Monad (replicateM, forM_, msum)
import Numeric (readHex)

readResp :: Handle -> IO SExp
readResp h = do
  hx <- replicateM 6 $ hGetChar h
  case readHex hx of
    ((n, ""):_) -> do
      sex <- replicateM n $ hGetChar h
      case parseSExp sex of
        Right r -> return r
        e -> error $ "unexpected parse: " ++ show e
    _ -> error $ "desynced from idris output: " ++ show hx

sendQuery :: Handle -> String -> Integer -> IO ()
sendQuery h q i = hPutStrLn h $ convSExp "interpret" q i

