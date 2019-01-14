{- --- Part Two ---
 -
 - Using the samples you collected, work out the number of each opcode and
 - execute the test program (the second section of your puzzle input).
 -
 - What value is contained in register 0 after executing the test program?
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
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data Register = Reg1 | Reg2 | Reg3 | Reg4 deriving (Eq, Ord, Show, Enum)

data Registers = Registers Word Word Word Word deriving (Eq, Ord, Show)

data InstructionType = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI
    | SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Eq, Ord, Show)

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

newtype UnknownProgram = UnknownProgram [UnknownInstruction]
  deriving (Eq, Ord, Show)

newtype Program = Program [Instruction]

data Input = Input [Sample] UnknownProgram deriving (Eq, Ord, Show)

handleInput :: Input -> IO ()
handleInput (Input samples unknownProgram) =
    case findProgram samples unknownProgram of
        Left err -> putStrLn err
        Right program -> print . regGet Reg1 . execAll $ program

findProgram :: [Sample] -> UnknownProgram -> Either String Program
findProgram samples unknown = applyProgramMapping mapping unknown
  where
    mapping = findProgramMapping samples

findProgramMapping
    :: [Sample]
    -> (UnknownInstruction -> Either String Instruction)
findProgramMapping samples = findProgramMapping' samples []

findProgramMapping'
    :: [Sample]
    -> [InstructionType]
    -> (UnknownInstruction -> Either String Instruction)
findProgramMapping' [] _ = \_ -> Left "Could not parse mapping."
findProgramMapping' samples exclude = case matchOne of
    [] -> \_ -> Left "Could not parse mapping."
    ones -> undefined
  where
    keyVals = assocs (findValid exclude) samples
    matchingOne = ((==) 1) . length . snd
    (matchOne, matchMultiple) = L.partition matchingOne keyVals

mapInstructionTo
    :: InstructionType
    -> Int
    -> (UnknownInstruction -> Maybe Instruction)
mapInstructionTo instructionType opcode = \uinstr ->
    if _opCode uinstr == opcode
    then
        let arg1 = _arg1 uinstr
            arg2 = _arg2 uinstr
            reg1 = toEnum . fromIntegral $ arg1
            reg2 = toEnum . fromIntegral $ arg2
            target = _target uinstr
        in case instructionType of
            ADDR -> Just $ Addr reg1 reg2 target
            ADDI -> Just $ Addi reg1 arg2 target
            MULR -> Just $ Mulr reg1 reg2 target
            MULI -> Just $ Muli reg1 arg2 target
            BANR -> Just $ Banr reg1 reg2 target
            BANI -> Just $ Bani reg1 arg2 target
            BORR -> Just $ Borr reg1 reg2 target
            BORI -> Just $ Bori reg1 arg2 target
            SETR -> Just $ Setr reg1 target
            SETI -> Just $ Seti arg1 target
            GTIR -> Just $ Gtir arg1 reg2 target
            GTRI -> Just $ Gtri reg1 arg2 target
            GTRR -> Just $ Gtrr reg1 reg2 target
            EQIR -> Just $ Eqir arg1 reg2 target
            EQRI -> Just $ Eqri reg1 arg2 target
            EQRR -> Just $ Eqrr reg1 reg2 target
    else Nothing

applyProgramMapping
    :: (UnknownInstruction -> Either String Instruction)
    -> UnknownProgram
    -> Either String Program
applyProgramMapping mapping (UnknownProgram unknown) =
    Program <$> M.mapM mapping unknown

findValid :: [InstructionType] -> Sample -> [Instruction]
findValid exclude (Sample before uninstr after)
    = filter ((==) after . exec before)
    . possibleInstructions exclude
    $ uninstr

execAll :: Program -> Registers
execAll (Program instructions) = foldl exec initialRegisters instructions
  where
    initialRegisters = Registers 0 0 0 0

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

getInstructionType :: Instruction -> InstructionType
getInstructionType (Addr _ _ _) = ADDR
getInstructionType (Addi _ _ _) = ADDI
getInstructionType (Mulr _ _ _) = MULR
getInstructionType (Muli _ _ _) = MULI
getInstructionType (Banr _ _ _) = BANR
getInstructionType (Bani _ _ _) = BANI
getInstructionType (Borr _ _ _) = BORR
getInstructionType (Bori _ _ _) = BORI
getInstructionType (Setr _ _) = SETR
getInstructionType (Seti _ _) = SETI
getInstructionType (Gtir _ _ _) = GTIR
getInstructionType (Gtri _ _ _) = GTRI
getInstructionType (Gtrr _ _ _) = GTRR
getInstructionType (Eqri _ _ _) = EQRI
getInstructionType (Eqir _ _ _) = EQIR
getInstructionType (Eqrr _ _ _) = EQRR

possibleInstructions :: [InstructionType] -> UnknownInstruction -> [Instruction]
possibleInstructions exclude (UnknownInstruction _ arg1 arg2 target) =
    filter (not . (\x -> elem x exclude) . getInstructionType)
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
