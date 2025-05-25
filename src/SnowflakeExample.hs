module SnowflakeExample where

import Data.Snowflake

main :: IO ()
main = do
  -- Create a new snowflake generator with default configuration and worker ID 0
  gen <- newSnowflakeGen defaultConfig 0

  -- Generate and print 20 snowflake IDs
  forM_ [1 .. 20] $ \_ -> do
    sid <- nextSnowflake gen
    print sid

-- To run this example:
-- 1. Add 'snowflake' to your cabal file's dependencies
-- 2. Compile with: ghc -o snowflake-example src/SnowflakeExample.hs
-- 3. Run with: ./snowflake-example
