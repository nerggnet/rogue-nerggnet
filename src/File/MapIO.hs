-- src/File/MapIO.hs
module File.MapIO (loadMapLevels, saveMapLevels) where

import File.Types
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B

-- Load map levels from a JSON file
loadMapLevels :: FilePath -> IO (Either String GameConfig)
loadMapLevels path = do
  content <- B.readFile path
  return $ eitherDecode content

-- Save map levels to a JSON file
saveMapLevels :: FilePath -> GameConfig -> IO ()
saveMapLevels path config = B.writeFile path (encode config)
