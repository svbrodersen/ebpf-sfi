module SFI where

import Data.List (sortOn)
import Ebpf.Asm
import Ebpf.Display ()
import Ebpf_cfg
import Prelude

type GraphList = [(Label, Trans, Label)]

getLabel :: CodeOffset -> Int -> Int
getLabel off l = fromIntegral off + l + 1

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
    Store _ reg' _ _ -> checkReg reg'
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
  -- Registers 1,2,10 are restricted by definition of the assignment. register 11 is used for guards
  _ <- checkRegisterUse lprog [1, 2, 10, 11]
  _ <- checkJumpInstructions lprog
  -- Now that we have a program that is nice, we can just return
  return ()

-- Require that r2 is a power of 2.
-- First we subtract 1 from r2, then we can use it as & r2
sfiAlgorithm :: LabeledProgram -> Maybe Program
sfiAlgorithm lprog = do
  _ <- checkPrerequisites lprog
  newProgram <- sfiAlgorithm' lprog 0
  Just $ map snd newProgram
 where
  sfiAlgorithm' :: LabeledProgram -> Int -> Maybe LabeledProgram
  sfiAlgorithm' lprog' curr =
    if curr >= length lprog'
      then Just lprog'
      else
        let (l, curr') = lprog' !! curr
         in case curr' of
              (Store _ dst off _) -> do
                -- In store we want to guard the destination
                (newProg, newCurr) <- handleMemloc l dst off
                sfiAlgorithm' newProg (newCurr + 1)
              (Load _ _ src off) -> do
                -- In load we want to guard the source
                (newProg, newCurr) <- handleMemloc l src off
                sfiAlgorithm' newProg (newCurr + 1)
              _ -> sfiAlgorithm' lprog' (curr + 1)
   where
    -- Get guard uses the old l as the start. That way we don't have to update any jumps, as we want them to jump to the start of the guard anyhow.
    getGuard l reg (Just off) =
      let ourReg = Reg 11
       in [ -- Move current value to r11, our controlled register
            (l, Binary B64 Mov ourReg (R reg))
          , -- subtract r1 from value
            (l + 1, Binary B64 Sub ourReg (R (Reg 1)))
          , -- Add the offset
            (l + 2, Binary B64 Add ourReg (Imm off))
          , -- And with r2, which has been sub 1 to get FFFFF for some value that is a power of 2
            (l + 3, Binary B64 And ourReg (R (Reg 2)))
          , -- Add back r1, such that we are within [r1, r1 + r2), instead of [0, r2)
            (l + 4, Binary B64 Add ourReg (R (Reg 1)))
          ]
    getGuard l reg Nothing =
      let ourReg = Reg 11
       in [ -- Move current value to r11, our controlled register
            (l, Binary B64 Mov ourReg (R reg))
          , -- subtract r1 from the value
            (l + 1, Binary B64 Sub ourReg (R (Reg 1)))
          , -- And with r2, which has been sub 1 to get FFFFF for some value that is a power of 2
            (l + 2, Binary B64 And ourReg (R (Reg 2)))
          , -- Add back r1, such that we are within [r1, r1 + r2), instead of [0, r2)
            (l + 3, Binary B64 Add ourReg (R (Reg 1)))
          ]

    handleMemloc :: Label -> Reg -> Maybe MemoryOffset -> Maybe (LabeledProgram, Int)
    handleMemloc l reg off =
      let guard = getGuard l reg off
          fixedProg = newLabels (length guard) l
          newProg = sortOn fst (fixedProg ++ guard)
       in Just (newProg, l + length guard)
    newLabels :: Int -> Label -> LabeledProgram
    newLabels len l = map (updateLabel l len) lprog'
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
            | n <= l && l' >= l = off - fromIntegral len
            | n > l && l' <= l = off + fromIntegral len
            -- Otherwise, the guard changes nothing.
            | otherwise = off
          newInstr = case instr of
            JCond cmp reg regimm _ ->
              JCond cmp reg regimm (fromIntegral newTarget)
            Jmp _ ->
              Jmp (fromIntegral newTarget)
            i -> i
       in (if l' >= l then l' + len else l', newInstr)
