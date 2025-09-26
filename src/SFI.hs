module SFI where

import Data.Maybe (catMaybes)
import Data.Set as Set
import Ebpf.Asm
import Ebpf.Display ()
import Ebpf_cfg
import Prelude

-- Check if either of the registers are ever used on the left hand side.
checkRegisterUse :: CFG -> [Int] -> Maybe ()
checkRegisterUse graph registers =
  let res = Set.foldl checkTrans False graph
   in (if res then Nothing else Just ())
 where
  checkTrans b (_, trans, _) = case trans of
    NonCF instr -> b || checkInst instr
    _ -> b
  checkInst inst = case inst of
    Binary _ _ reg' _ -> checkReg reg'
    Unary _ _ reg' -> checkReg reg'
    Store _ reg' _ _ -> checkReg reg'
    Load _ reg' _ _ -> checkReg reg'
    LoadImm reg' _ -> checkReg reg'
    LoadMapFd reg' _ -> checkReg reg'
    LoadInd _ reg' _ -> checkReg reg'
    JCond _ reg' _ _ -> checkReg reg'
    _ -> False
  checkReg (Reg n) = n `elem` registers

checkJumpInstructions :: CFG -> LabeledProgram -> Maybe ()
checkJumpInstructions graph prog =
  let res = Set.foldl checkTrans False graph
   in (if res then Nothing else Just ())
 where
  checkTrans b (_, trans, n) = case trans of
    Unconditional -> b || checkJump n
    Assert{} -> b || checkJump n
    _ -> b
  checkJump n = not $ any (\(i, _) -> i == n) prog

checkPrerequisites :: CFG -> LabeledProgram -> Maybe ()
checkPrerequisites graph prog = do
  -- Registers 1,2,10 are restricted by definition of the assignment. register 11 is used for guards
  _ <- checkRegisterUse graph [1, 2, 10, 11]
  _ <- checkJumpInstructions graph prog
  -- Now that we have a program that is nice, we can just return
  return ()

cfgToProgram :: CFG -> Program
cfgToProgram = undefined

sfiAlgorithm :: CFG -> LabeledProgram -> Maybe Program
sfiAlgorithm graph prog = do
  _ <- checkPrerequisites graph prog
  newGraph <- sfiAlgorithm' graph (Set.elemAt 0 graph)
  Just $ cfgToProgram newGraph
 where
  sfiAlgorithm' :: CFG -> (Label, Trans, Label) -> Maybe CFG
  sfiAlgorithm' graph' curr = undefined
