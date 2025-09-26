module Main where

import Data.Text as T
import Ebpf.AsmParser
import Ebpf.Display (displayProgram)
import SFI
import qualified System.Environment as Sys
import Text.Printf

main :: IO ()
main =
  do
    args <- Sys.getArgs
    case args of
      [ebpfFile, outFile] ->
        do
          res <- parseFromFile ebpfFile
          case res of
            Left err ->
              do
                putStrLn "Some sort of error occurred while parsing:"
                print err
            Right prog ->
              do
                printf "The eBPF file %s has %d instructions\n" ebpfFile (Prelude.length prog)
                let lprog = Prelude.zip [0 ..] prog
                    res' = SFI.sfiAlgorithm lprog
                 in case res' of
                      Nothing -> do
                        _ <- printf "Prerequisites failed\n"
                        return ()
                      Just safeProg -> do
                        _ <- writeFile outFile $ T.unpack $ displayProgram safeProg
                        _ <- printf "Wrote analysis results to %s\n" outFile
                        return ()
      _ ->
        putStrLn "Usage <EBPF_FILE> <OUT_FILE>"
