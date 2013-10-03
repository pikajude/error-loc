{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Prelude hiding (error)
-- > import ErrorLoc
-- >
-- > main :: IO ()
-- > main = $error "Oh no!"
--
-- > test.hs:7:10: Oh no!
module ErrorLoc (error) where

import Language.Haskell.TH
import Prelude hiding (error)
import qualified Prelude as P

-- | Provides a version of 'Prelude.error' with call-site metadata.
error :: Q Exp
error = do
    loc <- location
    [e|(.) P.error (concat [$(stringE (loc_filename loc))
                           , ":"
                           , $(stringE . show . fst $ loc_start loc)
                           , ":"
                           , $(stringE . show . snd $ loc_start loc)
                           , ": "] ++)|]
