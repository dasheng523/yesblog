module Ys.Pitch where

import Data.List (lookup)
import Reactive.Banana (
    Event,
    accumB,
    compile,
    filterJust,
    stepper,
 )
import Reactive.Banana.Frameworks (
    AddHandler,
    MomentIO,
    actuate,
    changes,
    fromAddHandler,
    newAddHandler,
    reactimate',
 )
import System.IO (hSetEcho)
import System.IO.Compat (getChar)

type Octave = Int

data Pitch = PA | PB | PC | PD | PE | PF | PG
    deriving stock (Eq, Enum, Show)

-- Mapping between pitch and the char responsible for it.
pitchChars :: [(Pitch, Char)]
pitchChars =
    [ (p, toEnum $ fromEnum 'a' + fromEnum p)
    | p <- [PA .. PG]
    ]

-- Reverse of pitchChars
charPitches :: [(Char, Pitch)]
charPitches = [(b, a) | (a, b) <- pitchChars]

data Note = Note Octave Pitch
    deriving stock (Show)

-- Filter and transform events at the same time.
filterMapJust :: (a -> Maybe b) -> Event a -> Event b
filterMapJust f = filterJust . fmap f

-- Change the original octave by adding a number of octaves, taking
-- care to limit the resulting octave to the 0..10 range.
changeOctave :: Int -> Octave -> Octave
changeOctave d = max 0 . min 10 . (d +)

-- Get the octave change for the '+' and '-' chars.
getOctaveChange :: Char -> Maybe Int
getOctaveChange c = case c of
    '+' -> Just 1
    '-' -> Just (-1)
    _ -> Nothing

makeNetworkDescription ::
    AddHandler Char ->
    MomentIO ()
makeNetworkDescription addKeyEvent = do
    eKey <- fromAddHandler addKeyEvent
    let
        eOctaveChange = filterMapJust getOctaveChange eKey
        ePitch = filterMapJust (`lookup` charPitches) eKey
    bOctave <- accumB 3 (changeOctave <$> eOctaveChange)
    bPitch <- stepper PC ePitch
    let
        bNote = Note <$> bOctave <*> bPitch
    eNoteChanged <- changes bNote
    reactimate' $
        fmap (\n -> putStrLn ("Now playing " ++ show n))
            <$> eNoteChanged

main2 :: IO ()
main2 = do
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)