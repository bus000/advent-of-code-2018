{- --- Day 19: Go With The Flow ---
 -
 - With the Elves well on their way constructing the North Pole base, you turn
 - your attention back to understanding the inner workings of programming the
 - device.
 -
 - You can't help but notice that the device's opcodes don't contain any flow
 - control like jump instructions. The device's manual goes on to explain:
 -
 - "In programs where flow control is required, the instruction pointer can be
 - bound to a register so that it can be manipulated directly. This way,
 - setr/seti can function as absolute jumps, addr/addi can function as relative
 - jumps, and other opcodes can cause truly fascinating effects."
 -
 - This mechanism is achieved through a declaration like #ip 1, which would
 - modify register 1 so that accesses to it let the program indirectly access
 - the instruction pointer itself. To compensate for this kind of binding, there
 - are now six registers (numbered 0 through 5); the five not bound to the
 - instruction pointer behave as normal. Otherwise, the same rules apply as the
 - last time you worked with this device.
 -
 - When the instruction pointer is bound to a register, its value is written to
 - that register just before each instruction is executed, and the value of that
 - register is written back to the instruction pointer immediately after each
 - instruction finishes execution. Afterward, move to the next instruction by
 - adding one to the instruction pointer, even if the value in the instruction
 - pointer was just updated by an instruction. (Because of this, instructions
 - must effectively set the instruction pointer to the instruction before the
 - one they want executed next.)
 -
 - The instruction pointer is 0 during the first instruction, 1 during the
 - second, and so on. If the instruction pointer ever causes the device to
 - attempt to load an instruction outside the instructions defined in the
 - program, the program instead immediately halts. The instruction pointer
 - starts at 0.
 -
 - It turns out that this new information is already proving useful: the CPU in
 - the device is not very powerful, and a background process is occupying most
 - of its time. You dump the background process' declarations and instructions
 - to a file (your puzzle input), making sure to use the names of the opcodes
 - rather than the numbers.
 -
 - For example, suppose you have the following program:
 -
 -    #ip 0
 -    seti 5 0 1
 -    seti 6 0 2
 -    addi 0 1 0
 -    addr 1 2 3
 -    setr 1 0 0
 -    seti 8 0 4
 -    seti 9 0 5
 -
 - When executed, the following instructions are executed. Each line contains
 - the value of the instruction pointer at the time the instruction started, the
 - values of the six registers before executing the instructions (in square
 - brackets), the instruction itself, and the values of the six registers after
 - executing the instruction (also in square brackets).
 -
 -    ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
 -    ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
 -    ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
 -    ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
 -    ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]
 -
 - In detail, when running this program, the following events occur:
 -
 - * The first line (#ip 0) indicates that the instruction pointer should be
 -   bound to register 0 in this program. This is not an instruction, and so the
 -   value of the instruction pointer does not change during the processing of
 -   this line.
 - * The instruction pointer contains 0, and so the first instruction is
 -   executed (seti 5 0 1). It updates register 0 to the current instruction
 -   pointer value (0), sets register 1 to 5, sets the instruction pointer to
 -   the value of register 0 (which has no effect, as the instruction did not
 -   modify register 0), and then adds one to the instruction pointer.
 - * The instruction pointer contains 1, and so the second instruction, seti 6 0
 -   2, is executed. This is very similar to the instruction before it: 6 is
 -   stored in register 2, and the instruction pointer is left with the value 2.
 - * The instruction pointer is 2, which points at the instruction addi 0 1 0.
 -   This is like a relative jump: the value of the instruction pointer, 2, is
 -   loaded into register 0. Then, addi finds the result of adding the value in
 -   register 0 and the value 1, storing the result, 3, back in register 0.
 -   Register 0 is then copied back to the instruction pointer, which will cause
 -   it to end up 1 larger than it would have otherwise and skip the next
 -   instruction (addr 1 2 3) entirely. Finally, 1 is added to the instruction
 -   pointer.
 - * The instruction pointer is 4, so the instruction setr 1 0 0 is run. This is
 -   like an absolute jump: it copies the value contained in register 1, 5, into
 -   register 0, which causes it to end up in the instruction pointer. The
 -   instruction pointer is then incremented, leaving it at 6.
 - * The instruction pointer is 6, so the instruction seti 9 0 5 stores 9 into
 -   register 5. The instruction pointer is incremented, causing it to point
 -   outside the program, and so the program ends.
 -
 - What value is left in register 0 when the background process halts?
 -}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import Data.Bits ((.&.), (.|.))
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V

main :: IO ()
main = preludeMain parseInput handleInput

data Register = Reg1 | Reg2 | Reg3 | Reg4 | Reg5 | Reg6
  deriving (Eq, Ord, Show, Enum)

data Registers = Registers !Word !Word !Word !Word !Word !Word
  deriving (Eq, Ord, Show)

data Instruction
    = Addr !Register !Register !Register
    | Addi !Register !Word !Register

    | Mulr !Register !Register !Register
    | Muli !Register !Word !Register

    | Banr !Register !Register !Register
    | Bani !Register !Word !Register

    | Borr !Register !Register !Register
    | Bori !Register !Word !Register

    | Setr !Register !Register
    | Seti !Word !Register

    | Gtir !Word !Register !Register
    | Gtri !Register !Word !Register
    | Gtrr !Register !Register !Register

    | Eqir !Word !Register !Register
    | Eqri !Register !Word !Register
    | Eqrr !Register !Register !Register
  deriving (Eq, Ord, Show)

data Program = Program
    { _ipReg        :: !Register
    , _instructions :: !(V.Vector Instruction)
    } deriving (Eq, Ord, Show)

handleInput :: Program -> IO ()
handleInput = print . regGet Reg1 . runProgram

runProgram :: Program -> Registers
runProgram (Program ipReg instructions) = go (Registers 0 0 0 0 0 0)
  where
    go registers = case loadInstruction registers of
        Nothing -> registers
        Just instruction -> go . incIp . exec registers $ instruction

    incIp registers = regSet ipReg registers (regGet ipReg registers + 1)

    loadInstruction registers =
        instructions V.!? fromIntegral (regGet ipReg registers)

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

regGet :: Register -> Registers -> Word
regGet Reg1 (Registers v _ _ _ _ _) = v
regGet Reg2 (Registers _ v _ _ _ _) = v
regGet Reg3 (Registers _ _ v _ _ _) = v
regGet Reg4 (Registers _ _ _ v _ _) = v
regGet Reg5 (Registers _ _ _ _ v _) = v
regGet Reg6 (Registers _ _ _ _ _ v) = v

regSet :: Register -> Registers -> Word -> Registers
regSet Reg1 (Registers _ v2 v3 v4 v5 v6) v = Registers v v2 v3 v4 v5 v6
regSet Reg2 (Registers v1 _ v3 v4 v5 v6) v = Registers v1 v v3 v4 v5 v6
regSet Reg3 (Registers v1 v2 _ v4 v5 v6) v = Registers v1 v2 v v4 v5 v6
regSet Reg4 (Registers v1 v2 v3 _ v5 v6) v = Registers v1 v2 v3 v v5 v6
regSet Reg5 (Registers v1 v2 v3 v4 _ v6) v = Registers v1 v2 v3 v4 v v6
regSet Reg6 (Registers v1 v2 v3 v4 v5 _) v = Registers v1 v2 v3 v4 v5 v

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = Program <$> (parseIp <* P.newline) <*> parseInstructions

parseIp :: P.Parsec T.Text () Register
parseIp = P.string "#ip " *> parseRegister

parseInstructions :: P.Parsec T.Text () (V.Vector Instruction)
parseInstructions = V.fromList <$> parseInstruction `P.endBy` P.newline

parseInstruction :: P.Parsec T.Text () Instruction
parseInstruction = P.count 5 P.anyChar >>= \case
    "addr " -> reg Addr
    "addi " -> imm Addi
    "mulr " -> reg Mulr
    "muli " -> imm Muli
    "banr " -> reg Banr
    "bani " -> imm Bani
    "borr " -> reg Borr
    "bori " -> imm Bori
    "gtir " -> immFlip Gtir
    "gtri " -> imm Gtri
    "gtrr " -> reg Gtrr
    "eqir " -> immFlip Eqir
    "eqri " -> imm Eqri
    "eqrr " -> reg Eqrr
    "setr " -> Setr <$> parseRegister <* P.space <* P.int <* P.space <*> parseRegister
    "seti " -> Seti <$> (P.int <* P.space <* P.int <* P.space) <*> parseRegister
    err -> fail $ "Unexpected instruction '" ++ err ++ "."
  where
    reg f = f <$>
        (parseRegister <* P.space) <*> (parseRegister <* P.space) <*> parseRegister
    imm f = f <$>
        (parseRegister <* P.space) <*> (P.int <* P.space) <*> parseRegister
    immFlip f = f <$>
        (P.int <* P.space) <*> (parseRegister <* P.space) <*> parseRegister

parseRegister :: P.Parsec T.Text () Register
parseRegister = P.satisfy (`elem` ("012345" :: String)) >>= \case
    '0' -> pure Reg1
    '1' -> pure Reg2
    '2' -> pure Reg3
    '3' -> pure Reg4
    '4' -> pure Reg5
    '5' -> pure Reg6
    n -> error $ "Unexpected character " ++ show n
