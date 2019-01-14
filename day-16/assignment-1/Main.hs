{- --- Day 16: Chronal Classification ---
 -
 - As you see the Elves defend their hot chocolate successfully, you go back to
 - falling through time. This is going to become a problem.
 -
 - If you're ever going to return to your own time, you need to understand how
 - this device on your wrist works. You have a little while before you reach
 - your next destination, and with a bit of trial and error, you manage to pull
 - up a programming manual on the device's tiny screen.
 -
 - According to the manual, the device has four registers (numbered 0 through
 - 3) that can be manipulated by instructions containing one of 16 opcodes. The
 - registers start with the value 0.
 -
 - Every instruction consists of four values: an opcode, two inputs (named A and
 - B), and an output (named C), in that order. The opcode specifies the behavior
 - of the instruction and how the inputs are interpreted. The output, C, is
 - always treated as a register.
 -
 - In the opcode descriptions below, if something says "value A", it means to
 - take the number given as A literally. (This is also called an "immediate"
 - value.) If something says "register A", it means to use the number given as A
 - to read from (or write to) the register with that number. So, if the opcode
 - addi adds register A and value B, storing the result in register C, and the
 - instruction addi 0 7 3 is encountered, it would add 7 to the value contained
 - by register 0 and store the sum in register 3, never modifying registers 0,
 - 1, or 2 in the process.
 -
 - Many opcodes are similar except for how they interpret their arguments. The
 - opcodes fall into seven general categories:
 -
 - Addition:
 -
 - * addr (add register) stores into register C the result of adding register A
 -   and register B.
 - * addi (add immediate) stores into register C the result of adding register A
 -   and value B.
 -
 - Multiplication:
 -
 - * mulr (multiply register) stores into register C the result of multiplying
 -   register A and register B.
 - * muli (multiply immediate) stores into register C the result of multiplying
 -   register A and value B.
 -
 - Bitwise AND:
 -
 - * banr (bitwise AND register) stores into register C the result of the
 -   bitwise AND of register A and register B.
 - * bani (bitwise AND immediate) stores into register C the result of the
 -   bitwise AND of register A and value B.
 -
 - Bitwise OR:
 -
 - * borr (bitwise OR register) stores into register C the result of the bitwise
 -   OR of register A and register B.
 - * bori (bitwise OR immediate) stores into register C the result of the
 -   bitwise OR of register A and value B.
 -
 - Assignment:
 -
 - * setr (set register) copies the contents of register A into register C.
 -   (Input B is ignored.)
 - * seti (set immediate) stores value A into register C. (Input B is ignored.)
 -
 - Greater-than testing:
 -
 - * gtir (greater-than immediate/register) sets register C to 1 if value A is
 -   greater than register B. Otherwise, register C is set to 0.
 - * gtri (greater-than register/immediate) sets register C to 1 if register A
 -   is greater than value B. Otherwise, register C is set to 0.
 - * gtrr (greater-than register/register) sets register C to 1 if register A is
 -   greater than register B. Otherwise, register C is set to 0.
 -
 - Equality testing:
 -
 - * eqir (equal immediate/register) sets register C to 1 if value A is equal to
 -   register B. Otherwise, register C is set to 0.
 - * eqri (equal register/immediate) sets register C to 1 if register A is equal
 -   to value B. Otherwise, register C is set to 0.
 - * eqrr (equal register/register) sets register C to 1 if register A is equal
 -   to register B. Otherwise, register C is set to 0.
 -
 - Unfortunately, while the manual gives the name of each opcode, it doesn't
 - seem to indicate the number. However, you can monitor the CPU to see the
 - contents of the registers before and after instructions are executed to try
 - to work them out. Each opcode has a number from 0 through 15, but the manual
 - doesn't say which is which. For example, suppose you capture the following
 - sample:
 -
 - Before: [3, 2, 1, 1]
 - 9 2 1 2
 - After:  [3, 2, 2, 1]
 -
 - This sample shows the effect of the instruction 9 2 1 2 on the registers.
 - Before the instruction is executed, register 0 has value 3, register 1
 - has value 2, and registers 2 and 3 have value 1. After the instruction is
 - executed, register 2's value becomes 2.
 -
 - The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2,
 - B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but
 - only three of them behave in a way that would cause the result shown in the
 - sample:
 -
 - * Opcode 9 could be mulr: register 2 (which has a value of 1) times register
 -   1 (which has a value of 2) produces 2, which matches the value stored in
 -   the output register, register 2.
 - * Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1
 -   produces 2, which matches the value stored in the output register, register
 -   2.
 - * Opcode 9 could be seti: value 2 matches the value stored in the output
 -   register, register 2; the number given for B is irrelevant.
 -
 - None of the other opcodes produce the result captured in the sample. Because
 - of this, the sample above behaves like three opcodes.
 -
 - You collect many of these samples (the first section of your puzzle input).
 - The manual also includes a small test program (the second section of your
 - puzzle input) - you can ignore it for now.
 -
 - Ignoring the opcode numbers, how many samples in your puzzle input behave
 - like three or more opcodes?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Main where

import AdventOfCode
import qualified Control.Monad as M
import Data.Bits ((.&.), (.|.))
import qualified Data.Text.Lazy as LT
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data Register = Reg1 | Reg2 | Reg3 | Reg4 deriving (Eq, Ord, Show, Enum)

data Registers = Registers Word Word Word Word deriving (Eq, Ord, Show)

data Instruction
    = Addr Register Register Register
    | Addi Register Word Register

    | Mulr Register Register Register
    | Muli Register Word Register

    | Banr Register Register Register
    | Bani Register Word Register

    | Borr Register Register Register
    | Bori Register Word Register

    | Setr Register Register
    | Seti Word Register

    | Gtir Word Register Register
    | Gtri Register Word Register
    | Gtrr Register Register Register

    | Eqir Word Register Register
    | Eqri Register Word Register
    | Eqrr Register Register Register
  deriving (Eq, Ord, Show)

data UnknownInstruction = UnknownInstruction
    { _opCode :: Word
    , _arg1   :: Word
    , _arg2   :: Word
    , _target :: Register
    }
  deriving (Eq, Ord, Show)

data Sample = Sample
    { _stateBefore :: Registers
    , _instruction :: UnknownInstruction
    , _stateAfter  :: Registers
    }
  deriving (Eq, Ord, Show)

data UnknownProgram = UnknownProgram [UnknownInstruction]
  deriving (Eq, Ord, Show)

data Input = Input [Sample] UnknownProgram deriving (Eq, Ord, Show)

handleInput :: Input -> IO ()
handleInput (Input samples _)
    = print
    . length
    . filter ((\x -> x >= 3) . length)
    . map findValid
    $ samples

findValid :: Sample -> [Instruction]
findValid (Sample before uninstr after)
    = filter ((==) after . exec before)
    . possibleInstructions
    $ uninstr

exec :: Registers -> Instruction -> Registers
exec registers (Addr reg1 reg2 target) = regSet target registers $ arg1 + arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

exec registers (Addi reg1 arg2 target) = regSet target registers $ arg1 + arg2
  where
    arg1 = regGet reg1 registers

exec registers (Mulr reg1 reg2 target) = regSet target registers $ arg1 * arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

exec registers (Muli reg1 arg2 target) = regSet target registers $ arg1 * arg2
  where
    arg1 = regGet reg1 registers

exec registers (Banr reg1 reg2 target) = regSet target registers $ arg1 .&. arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

exec registers (Bani reg1 arg2 target) = regSet target registers $ arg1 .&. arg2
  where
    arg1 = regGet reg1 registers

exec registers (Borr reg1 reg2 target) = regSet target registers $ arg1 .|. arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

exec registers (Bori reg1 arg2 target) = regSet target registers $ arg1 .|. arg2
  where
    arg1 = regGet reg1 registers

exec registers (Setr reg1 target) = regSet target registers arg1
  where
    arg1 = regGet reg1 registers

exec registers (Seti arg1 target) = regSet target registers arg1

exec registers (Gtir arg1 reg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 > arg2
  where
    arg2 = regGet reg2 registers

exec registers (Gtri reg1 arg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 > arg2
  where
    arg1 = regGet reg1 registers

exec registers (Gtrr reg1 reg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 > arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

exec registers (Eqir arg1 reg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 == arg2
  where
    arg2 = regGet reg2 registers

exec registers (Eqri reg1 arg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 == arg2
  where
    arg1 = regGet reg1 registers

exec registers (Eqrr reg1 reg2 target) =
    regSet target registers . fromIntegral . fromEnum $ arg1 == arg2
  where
    arg1 = regGet reg1 registers
    arg2 = regGet reg2 registers

possibleInstructions :: UnknownInstruction -> [Instruction]
possibleInstructions (UnknownInstruction _ arg1 arg2 target) =
    [ Addr reg1 reg2 target
    , Addi reg1 arg2 target

    , Mulr reg1 reg2 target
    , Muli reg1 arg2 target

    , Banr reg1 reg2 target
    , Bani reg1 arg2 target

    , Borr reg1 reg2 target
    , Bori reg1 arg2 target

    , Setr reg1 target
    , Seti arg1 target

    , Gtir arg1 reg2 target
    , Gtri reg1 arg2 target
    , Gtrr reg1 reg2 target

    , Eqir arg1 reg2 target
    , Eqri reg1 arg2 target
    , Eqrr reg1 reg2 target
    ]
  where
    reg1 = toEnum . fromIntegral $ arg1
    reg2 = toEnum . fromIntegral $ arg2

regGet :: Register -> Registers -> Word
regGet Reg1 (Registers v _ _ _) = v
regGet Reg2 (Registers _ v _ _) = v
regGet Reg3 (Registers _ _ v _) = v
regGet Reg4 (Registers _ _ _ v) = v

regSet :: Register -> Registers -> Word -> Registers
regSet Reg1 (Registers _ v2 v3 v4) v = Registers v v2 v3 v4
regSet Reg2 (Registers v1 _ v3 v4) v = Registers v1 v v3 v4
regSet Reg3 (Registers v1 v2 _ v4) v = Registers v1 v2 v v4
regSet Reg4 (Registers v1 v2 v3 _) v = Registers v1 v2 v3 v

parseInput :: LT.Text -> Either P.ParseError Input
parseInput = P.parse (parseInput' <* P.eof) ""

parseInput' :: P.Parsec LT.Text () Input
parseInput' = Input <$> parseSamples <* P.string "\n\n" <*> parseUnknownProgram

parseSamples :: P.Parsec LT.Text () [Sample]
parseSamples = parseSample `P.endBy` P.string "\n\n"

parseUnknownProgram :: P.Parsec LT.Text () UnknownProgram
parseUnknownProgram = UnknownProgram
    <$> parseUnknownInstruction `P.endBy` P.char '\n'

parseSample :: P.Parsec LT.Text () Sample
parseSample = Sample
    <$> (P.string "Before: " *> parseRegisters <* P.char '\n')
    <*> (parseUnknownInstruction <* P.char '\n')
    <*> (P.string "After:  " *> parseRegisters)

parseRegisters :: P.Parsec LT.Text () Registers
parseRegisters = do
    M.void $ P.char '['
    v1 <- P.int
    M.void $ P.string ", "
    v2 <- P.int
    M.void $ P.string ", "
    v3 <- P.int
    M.void $ P.string ", "
    v4 <- P.int
    M.void $ P.char ']'

    return $! Registers v1 v2 v3 v4

parseUnknownInstruction :: P.Parsec LT.Text () UnknownInstruction
parseUnknownInstruction = UnknownInstruction
    <$> P.int <* P.char ' '
    <*> P.int <* P.char ' '
    <*> P.int <* P.char ' '
    <*> parseRegister

parseRegister :: P.Parsec LT.Text () Register
parseRegister = P.satisfy (`elem` ("0123" :: String)) >>= \case
    '0' -> pure Reg1
    '1' -> pure Reg2
    '2' -> pure Reg3
    '3' -> pure Reg4
    n -> error $ "Unexpected character " ++ show n
