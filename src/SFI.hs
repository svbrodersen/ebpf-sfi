module SFI where

import Data.List (sortOn)
import Ebpf.Asm
import Ebpf.Display ()
import Ebpf_cfg
import Prelude

type GraphList = [(Label, Trans, Label)]

getLabel :: CodeOffset -> Int -> Int
getLabel off l = fromIntegral off + l

checkPrerequisite :: ((Int, Instruction) -> Bool) -> LabeledProgram -> Maybe ()
checkPrerequisite p lprog =
  let res = any p lprog
   in (if res then Nothing else Just ())

-- Check if either of the registers are ever used on the left hand side.
checkRegisterUse :: LabeledProgram -> [Int] -> Maybe ()
checkRegisterUse lprog registers = checkPrerequisite checkInst lprog
 where
  checkInst (_, inst) = case inst of
    Binary _ _ reg' _ -> checkReg reg'
    Unary _ _ reg' -> checkReg reg'
    -- Store just stores at the memory location of reg, so we don't have to do anything
    Store{} -> False
    Load _ reg' _ _ -> checkReg reg'
    LoadImm reg' _ -> checkReg reg'
    LoadMapFd reg' _ -> checkReg reg'
    LoadInd _ reg' _ -> checkReg reg'
    JCond _ reg' _ _ -> checkReg reg'
    _ -> False
  checkReg (Reg n) = n `elem` registers

checkJumpInstructions :: LabeledProgram -> Maybe ()
checkJumpInstructions lprog = checkPrerequisite checkInst lprog
 where
  checkInst (l, instr) = case instr of
    JCond _ _ _ off -> checkJump off l
    Jmp off -> checkJump off l
    -- Do not allow any calls to extern
    (Call _) -> True
    _ -> False
  checkJump off l =
    let
      res = any (\(i', _) -> i' == getLabel off l) lprog
     in
      not res

checkPrerequisites :: LabeledProgram -> Maybe ()
checkPrerequisites lprog = do
  -- Registers 1,2,10 are restricted by definition of the assignment. register 9, 11 is used for guards
  _ <- checkRegisterUse lprog [1, 2, 9, 10, 11]
  _ <- checkJumpInstructions lprog
  -- Now that we have a program that is nice, we can just return
  return ()

-- Subtracts 1 from r2, as r2 is a power of 2
insertR2Sub :: LabeledProgram -> Maybe LabeledProgram
insertR2Sub lprog =
  let lprog' = map (\(i, x) -> (i + 1, x)) lprog
      r2Instr = (0, Binary B64 Sub (Reg 2) (Imm 1))
   in Just $ r2Instr : lprog'

-- Require that r2 is a power of 2.
-- First we subtract 1 from r2, then we can use it as & r2
sfiAlgorithm :: LabeledProgram -> Maybe Program
sfiAlgorithm lprog' = do
  _ <- checkPrerequisites lprog'
  lprog <- insertR2Sub lprog'
  newProgram <- sfiAlgorithm' lprog 0
  Just $ map snd newProgram
 where
  sfiAlgorithm' :: LabeledProgram -> Int -> Maybe LabeledProgram
  sfiAlgorithm' cur_prog curr =
    if curr >= length cur_prog
      then Just cur_prog
      else
        let (l, curr') = cur_prog !! curr
         in case curr' of
              i@(Store _ dst off _) -> do
                -- In store we want to guard the destination
                (newProg, newCurr) <- handleMemloc l dst off i
                -- After adding the guard, we now replace Reg and off with reg 11
                sfiAlgorithm' newProg (newCurr + 1)
              i@(Load _ _ src off) -> do
                -- In load we want to guard the source
                (newProg, newCurr) <- handleMemloc l src off i
                sfiAlgorithm' newProg (newCurr + 1)
              _ -> sfiAlgorithm' cur_prog (curr + 1)
   where
    handleInstructionGuard l' newReg (Store s _ _ regimm) =
      (l', Store s newReg Nothing regimm)
    handleInstructionGuard l' newReg (Load s dst _ _) =
      (l', Load s dst newReg Nothing)
    handleInstructionGuard l' _ _ =
      -- This case should never happen
      (l', Exit)

    -- Get guard uses the old l as the start. That way we don't have to update any jumps, as we want them to jump to the start of the guard anyhow.
    getGuard l reg (Just off) i =
      let r11 = Reg 11
          r9 = Reg 9
          r10 = Reg 10
          r1 = Reg 1
          r2 = Reg 2
       in [ -- Move current value to r9
            (l, Binary B64 Mov r9 (R reg))
          , -- Add the offset
            (l + 1, Binary B64 Add r9 (Imm off))
          , -- Copy the result after offset in r11
            (l + 2, Binary B64 Mov r11 (R r9))
          , -- subtract r10 from value
            (l + 3, Binary B64 Sub r9 (R r10))
          , -- And with 511
            (l + 4, Binary B64 And r9 (Imm 511))
          , -- Add back r10, so we go from [0, 511] to [r10, r10 + 511]
            (l + 5, Binary B64 Add r9 (R r10))
          , -- if there is no difference between this and the value in r11, then this memory location is between [r10, r10 + 512),
            -- So we insert a jump to jump over the next guard. The jump is 5,
            (l + 6, JCond Jeq r9 (R r11) 6)
          , (l + 7, Binary B64 Sub r11 (R r1))
          , -- And with 511
            (l + 8, Binary B64 And r11 (R r2))
          , -- Add back r10, so we go from [0, 511] to [r10, r10 + 511]
            (l + 9, Binary B64 Add r11 (R r1))
          , -- Now we change the original instruction to be with r11
            handleInstructionGuard (l + 10) r11 i
          , (l + 11, Jmp 2)
          , -- This is where we jump to, if we found that r9 and r11 were the same before.
            handleInstructionGuard (l + 12) r9 i
          ]
    getGuard l reg Nothing i =
      let r11 = Reg 11
          r9 = Reg 9
          r10 = Reg 10
          r1 = Reg 1
          r2 = Reg 2
       in [ -- Move current value to r9
            (l, Binary B64 Mov r9 (R reg))
          , -- Copy to r11
            (l + 1, Binary B64 Mov r11 (R r9))
          , -- subtract r10 from value
            (l + 2, Binary B64 Sub r9 (R r10))
          , -- And with 511
            (l + 3, Binary B64 And r9 (Imm 511))
          , -- Add back r10, so we go from [0, 511] to [r10, r10 + 511]
            (l + 4, Binary B64 Add r9 (R r10))
          , -- if there is no difference between this and the value in r11, then this memory location is between [r10, r10 + 512),
            -- So we insert a jump to jump over the next guard. The jump is 5,
            (l + 5, JCond Jeq r9 (R r11) 6)
          , (l + 6, Binary B64 Sub r11 (R r1))
          , -- And with 511
            (l + 7, Binary B64 And r11 (R r2))
          , -- Add back r10, so we go from [0, 511] to [r10, r10 + 511]
            (l + 8, Binary B64 Add r11 (R r1))
          , -- Now we change the original instruction to be with r11
            handleInstructionGuard (l + 9) r11 i
          , (l + 10, Jmp 2)
          , -- This is where we jump to, if we found that r9 and r11 were the same before.
            handleInstructionGuard (l + 11) r9 i
          ]
    removeLabel _ [] = []
    removeLabel l ((k, x) : xs)
      | l == k = xs
      | otherwise = (k, x) : removeLabel l xs

    handleMemloc :: Label -> Reg -> Maybe MemoryOffset -> Instruction -> Maybe (LabeledProgram, Int)
    handleMemloc l reg off i =
      -- First create the guard
      let guard = getGuard l reg off i
          -- Remove the original possibly offending statement
          lprog'' = removeLabel l cur_prog
          -- Increment all labels, and make sure our jumps are correct
          fixedProg = newLabels (length guard) l lprog''
          -- Add the guard to the program, and then sort
          newProg = sortOn fst (fixedProg ++ guard)
       in Just (newProg, l + length guard - 1)
    newLabels :: Int -> Label -> LabeledProgram -> LabeledProgram
    newLabels len l = map (updateLabel l len)
    updateLabel :: Label -> Int -> (Int, Instruction) -> (Int, Instruction)
    updateLabel l len (l', instr) =
      -- n is the label the instr wants to jump to (in case of jump)
      let (n, off) = case instr of
            JCond _ _ _ code -> (getLabel code l', code)
            Jmp code -> (getLabel code l', code)
            _ -> (0, 0) -- dummy, won't be used
            -- New target for jump instructions
          newTarget
            -- If we were jumping over the current label, then we now have to
            -- take the guard into account. (Also irrelevant to use >=, as the
            -- current l witll never be a jump anyhow.)
            -- Also Also, in the case we are jumping from somewhere after, we
            -- want to update it to instead jump to the guard. This is why n ==
            -- l is included in the n <=.
            | n <= l && l' >= l = off - (fromIntegral len - 1)
            | n > l && l' <= l = off + (fromIntegral len - 1)
            -- Otherwise, the guard changes nothing.
            | otherwise = off
          newInstr = case instr of
            JCond cmp reg regimm _ ->
              JCond cmp reg regimm (fromIntegral newTarget)
            Jmp _ ->
              Jmp (fromIntegral newTarget)
            i -> i
       in (if l' >= l then l' + len - 1 else l', newInstr)
