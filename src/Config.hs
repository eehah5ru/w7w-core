{-# LANGUAGE OverloadedStrings #-}

module W7W.Config where

import           Data.Default (def)
import qualified Text.Sass.Options as SO
import Hakyll

--
-- default config. Can be overriden
--
config :: Configuration
config =
  defaultConfiguration {destinationDirectory = "_site"
                       ,storeDirectory = "_cache"
                       ,tmpDirectory = "_tmp"
                       ,previewPort = 8001
                       ,previewHost = "0.0.0.0"
                       ,inMemoryCache = True }


--
-- default SASS options
--
sassOptions :: SO.SassOptions
sassOptions = def { SO.sassIncludePaths = Just [ "css/"
                                               , "bower_components/foundation-sites/scss/"] 
                  , SO.sassOutputStyle = SO.SassStyleCompressed}
