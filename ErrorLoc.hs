{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module ErrorLoc (
  errorLoc
) where

import Language.Haskell.TH

-- | Provides a version of 'error' with call-site metadata.
errorLoc :: Q Exp
errorLoc = do
    loc <- location
    [e|(.) error (concat [$(stringE (loc_filename loc))
                       , ":"
                       , $(stringE . show . fst $ loc_start loc)
                       , ":"
                       , $(stringE . show . snd $ loc_start loc)
                       , ": "] ++)|]
