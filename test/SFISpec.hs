module SFISpec (spec) where

import qualified Data.Set as Set
import Ebpf.Asm
import Ebpf_cfg
import SFI
import Test.Hspec

-- Sample program that uses a restricted register (R1)
programWithRestrictedRegister :: Program
programWithRestrictedRegister =
  [ Binary B64 Add (Reg 1) (Reg 2)
  , Exit
  ]

-- Sample program that does not use restricted registers
programWithoutRestrictedRegister :: Program
programWithoutRestrictedRegister =
  [ Binary B64 Add (Reg 3) (Reg 4)
  , Exit
  ]

-- Sample program with an invalid jump
programWithInvalidJump :: LabeledProgram
programWithInvalidJump =
  [ (Jmp 3)
  , (Exit)
  ]

-- Sample program with only valid jumps
programWithValidJump :: LabeledProgram
programWithValidJump =
  [ (Jmp 2)
  , (Exit)
  ]

spec :: Spec
spec = do
  describe "SFI Prerequisite Checks" $ do
    it "checkRegisterUse should return Nothing for program with restricted register" $ do
      let cfg = Ebpf_cfg.cfg programWithRestrictedRegister
      checkRegisterUse cfg [1, 2, 10, 11] `shouldBe` Nothing

    it "checkRegisterUse should return Just () for program without restricted register" $ do
      let cfg = Ebpf_cfg.cfg programWithoutRestrictedRegister
      checkRegisterUse cfg [1, 2, 10, 11] `shouldBe` Just ()

    it "checkJumpInstructions should return Nothing for program with invalid jump" $ do
      let cfg = Ebpf_cfg.cfg $ programWithInvalidJump
          labeledProg = zip [1..] programWithInvalidJump
        in checkJumpInstructions cfg programWithInvalidJump `shouldBe` Nothing

    it "checkJumpInstructions should return Just () for program with valid jump" $ do
      let cfg = Ebpf_cfg.cfg $ map snd programWithValidJump
        in checkJumpInstructions cfg programWithValidJump `shouldBe` Just ()

    it "checkPrerequisites should return Nothing for program with restricted register" $ do
      let prog = programWithRestrictedRegister
          labeledProg = zip [1 ..] prog
          cfg = Ebpf_cfg.cfg prog
        in checkPrerequisites cfg labeledProg `shouldBe` Nothing

    it "checkPrerequisites should return Nothing for program with invalid jump" $ do
      let cfg = Ebpf_cfg.cfg programWithInvalidJump
          labeledProg = zip [1..] programWithInvalidJump
        in checkPrerequisites cfg labeledProg `shouldBe` Nothing

    it "checkPrerequisites should return Just () for a valid program" $ do
      let prog = programWithoutRestrictedRegister
           labeledProg = zip [1 ..] prog
           cfg = Ebpf_cfg.cfg prog
        in checkPrerequisites cfg labeledProg `shouldBe` Just ()

  describe "SFI Algorithm" $ do
    it "sfiAlgorithm should correctly instrument a program" $ do
      pendingWith "sfiAlgorithm not implemented"
