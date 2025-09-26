module SFISpec (spec) where

import Ebpf.Asm
import Ebpf_cfg
import SFI
import Test.Hspec

-- Sample program that uses a restricted register (R1)
programWithRestrictedRegister :: Program
programWithRestrictedRegister =
  [ Binary B64 Add (Reg 1) (R (Reg 2))
  , Exit
  ]

-- Sample program that does not use restricted registers
programWithoutRestrictedRegister :: Program
programWithoutRestrictedRegister =
  [ Binary B64 Add (Reg 3) (R (Reg 4))
  , Exit
  ]

-- Sample program with an invalid jump
programWithInvalidJump :: Program
programWithInvalidJump =
  [ Jmp 2
  , Exit
  ]

-- Sample program with only valid jumps
programWithValidJump :: Program
programWithValidJump =
  [ Jmp 0
  , Exit
  ]

spec :: Spec
spec = do
  describe "SFI Prerequisite Checks" $ do
    it "checkRegisterUse should return Nothing for program with restricted register" $ do
      let cfg' = Ebpf_cfg.cfg programWithRestrictedRegister
      checkRegisterUse cfg' [1, 2, 10, 11] `shouldBe` Nothing

    it "checkRegisterUse should return Just () for program without restricted register" $ do
      let cfg' = Ebpf_cfg.cfg programWithoutRestrictedRegister
      checkRegisterUse cfg' [1, 2, 10, 11] `shouldBe` Just ()

    it "checkJumpInstructions should return Nothing for program with invalid jump" $ do
      let labeledProg = zip [0 ..] programWithInvalidJump
          cfg' = Ebpf_cfg.cfg programWithInvalidJump
      checkJumpInstructions cfg' labeledProg `shouldBe` Nothing

    it "checkJumpInstructions should return Just () for program with valid jump" $ do
      let labeledProg = zip [0 ..] programWithValidJump
          cfg' = Ebpf_cfg.cfg programWithValidJump
      checkJumpInstructions cfg' labeledProg `shouldBe` Just ()

    it "checkPrerequisites should return Nothing for program with restricted register" $ do
      let prog = programWithRestrictedRegister
          labeledProg = zip [0 ..] prog
          cfg' = Ebpf_cfg.cfg prog
      checkPrerequisites cfg' labeledProg `shouldBe` Nothing

    it "checkPrerequisites should return Nothing for program with invalid jump" $ do
      let prog = programWithInvalidJump
          labeledProg = zip [0 ..] prog
          cfg' = Ebpf_cfg.cfg prog
      checkPrerequisites cfg' labeledProg `shouldBe` Nothing

    it "checkPrerequisites should return Just () for a valid program" $ do
      let prog = programWithoutRestrictedRegister
          labeledProg = zip [0 ..] prog
          cfg' = Ebpf_cfg.cfg prog
      checkPrerequisites cfg' labeledProg `shouldBe` Just ()

  describe "SFI Algorithm" $ do
    it "sfiAlgorithm should correctly instrument a program" $ do
      pendingWith "sfiAlgorithm not implemented"
