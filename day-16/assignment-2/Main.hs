{- --- Part Two ---
 -
 - Using the samples you collected, work out the number of each opcode and
 - execute the test program (the second section of your puzzle input).
 -
 - What value is contained in register 0 after executing the test program?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import qualified Control.Monad as M
import Data.Bits ((.&.), (.|.))
import qualified Data.Text.Lazy as LT
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L
import qualified Data.Map as Map
import qualified System.Exit as S

main :: IO ()
main = defaultMain parseInput handleInput

data Register = Reg1 | Reg2 | Reg3 | Reg4 deriving (Eq, Ord, Show, Enum)

data Registers = Registers !Word !Word !Word !Word deriving (Eq, Ord, Show)

data InstructionType = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
    | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Show, Eq, Ord)

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

data UnknownInstruction = UnknownInstruction
    { _opCode :: !Word
    , _arg1   :: !Word
    , _arg2   :: !Word
    , _target :: !Register
    }
  deriving (Eq, Ord, Show)

data Sample = Sample
    { _stateBefore :: !Registers
    , _instruction :: !UnknownInstruction
    , _stateAfter  :: !Registers
    }
  deriving (Eq, Ord, Show)

data UnknownProgram = UnknownProgram [UnknownInstruction]
  deriving (Eq, Ord, Show)

data Program = Program [Instruction] deriving (Eq, Ord, Show)

data Input = Input [Sample] UnknownProgram deriving (Eq, Ord, Show)

handleInput :: Input -> IO ()
handleInput (Input samples unknownProgram) = case program of
    Left err -> S.die err
    Right p -> print . regGet Reg1 . runProgram $ p
  where
    instructionMap = Map.fromList . determineMapping . map findValid $ samples
    program = parseProgram instructionMap unknownProgram

determineMapping :: [(Word, [InstructionType])] -> [(Word, InstructionType)]
determineMapping samples = case L.find ((== 1) . length . snd) samples of
    Just (opCode, [instr]) -> (opCode, instr) :
        (determineMapping
        . filter (not . null . snd)
        . map (removeInstr instr)
        . filter ((/= opCode) . fst)
        $ samples)
    _ -> []
  where
    removeInstr instr (op, instrs) = (op, filter (/= instr) instrs)

parseProgram :: Map.Map Word InstructionType -> UnknownProgram -> Either String Program
parseProgram instructions (UnknownProgram program) =
    Program <$> mapM parseInstruction program
  where
    parseInstruction :: UnknownInstruction -> Either String Instruction
    parseInstruction ui = case Map.lookup (_opCode ui) instructions of
        Just ADDR -> register Addr ui
        Just ADDI -> immediate Addi ui
        Just MULR -> register Mulr ui
        Just MULI -> immediate Muli ui
        Just BANR -> register Banr ui
        Just BANI -> immediate Bani ui
        Just BORR -> register Borr ui
        Just BORI -> immediate Bori ui
        Just SETR -> Setr <$> computeRegister (_arg1 ui) <*> pure (_target ui)
        Just SETI -> pure $ Seti (_arg1 ui) (_target ui)
        Just GTIR -> flippedImmediate Gtir ui
        Just GTRI -> immediate Gtri ui
        Just GTRR -> register Gtrr ui
        Just EQIR -> flippedImmediate Eqir ui
        Just EQRI -> immediate Eqri ui
        Just EQRR -> register Eqrr ui
        Nothing -> Left $ "Could not find instruction for opcode "
            ++ show (_opCode ui)

    register
        :: (Register -> Register -> Register -> Instruction)
        -> UnknownInstruction
        -> Either String Instruction
    register f (UnknownInstruction _ arg1 arg2 target) =
        f <$> computeRegister arg1 <*> computeRegister arg2 <*> pure target

    immediate
        :: (Register -> Word -> Register -> Instruction)
        -> UnknownInstruction
        -> Either String Instruction
    immediate f (UnknownInstruction _ arg1 arg2 target) =
        f <$> computeRegister arg1 <*> pure arg2 <*> pure target

    flippedImmediate
        :: (Word -> Register -> Register -> Instruction)
        -> UnknownInstruction
        -> Either String Instruction
    flippedImmediate f (UnknownInstruction _ arg1 arg2 target) =
        f <$> pure arg1 <*> computeRegister arg2 <*> pure target

findValid :: Sample -> (Word, [InstructionType])
findValid (Sample before uninstr after) = (_opCode uninstr, matches)
  where
    matches
        = map instructionType
        . filter ((==) after . exec before)
        . possibleInstructions
        $ uninstr

instructionType :: Instruction -> InstructionType
instructionType Addr{} = ADDR
instructionType Addi{} = ADDI
instructionType Mulr{} = MULR
instructionType Muli{} = MULI
instructionType Banr{} = BANR
instructionType Bani{} = BANI
instructionType Borr{} = BORR
instructionType Bori{} = BORI
instructionType Setr{} = SETR
instructionType Seti{} = SETI
instructionType Gtir{} = GTIR
instructionType Gtri{} = GTRI
instructionType Gtrr{} = GTRR
instructionType Eqir{} = EQIR
instructionType Eqri{} = EQRI
instructionType Eqrr{} = EQRR

runProgram :: Program -> Registers
runProgram (Program instructions) = L.foldl' exec (Registers 0 0 0 0) instructions

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

computeRegister :: (Integral a, Show a) => a -> Either String Register
computeRegister 0 = Right Reg1
computeRegister 1 = Right Reg2
computeRegister 2 = Right Reg3
computeRegister 3 = Right Reg4
computeRegister n = Left $ "No register for number " ++ show n ++ "."

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
