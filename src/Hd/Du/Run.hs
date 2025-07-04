module Hd.Du.Run where

import Options.Applicative as Opt
import TextShow hiding (fromString)

import Hd.Du.App
import Hd.Du.DirTree
import Hd.Du.DiskUsage
import Hd.Du.FileCounter
import Prelude hiding (toText)

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
    unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: (TextShow s) => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2 * n) ' '

work :: AppConfig -> IO ()
work config = do
    (_, dirs) <- runMyApp dirTree config ()
    (_, counters) <- runMyApp fileCount config ()
    (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)

    putText $
        toText $
            buildEntries "Directory tree:" treeEntryBuilder dirs
                <> buildEntries "File counter:" tabEntryBuilder counters
                <> buildEntries "File space usage:" tabEntryBuilder usages

main :: IO ()
main = execParser opts >>= work
  where
    opts =
        info
            (mkConfig <**> helper)
            (fullDesc <> progDesc "Directory usage info")

mkConfig :: Opt.Parser AppConfig
mkConfig =
    AppConfig
        <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
        <*> option
            auto
            ( metavar "DEPTH"
                <> short 'd'
                <> long "depth"
                <> value maxBound
                <> help "Display an entry for all directories DEPTH derectories deep"
            )
        <*> optional
            (strOption (metavar "EXT" <> long "extension" <> short 'e' <> help "Filter files by extension"))
        <*> switch
            (short 'L' <> help "Follow symlinks (OFF by default)")