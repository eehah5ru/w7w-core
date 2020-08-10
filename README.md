# W7W is a shared hakyll engine which I'm using in different projects

# how to start a new project:

## get source code skeleton
- create new dir for the project: `stack new <pkg-name> --bare rio`
- add submodule w7w: git@github.com:eehah5ru/w7w-core.git
- `cd src && ln -s ../w7w W7W`

## modify stack.yml

- add `extra-deps`
```yaml
extra-deps:
  - hakyll-4.13.3.0
  - hakyll-sass-0.2.4
  - hlibsass-0.1.8.0
```

- add `flags` for hlibsass dependency
```yaml
flags:
  hlibsass:
    #sharedlibsass: true
    externallibsass: true
```

- add `extra-include-dirs`
```yaml
extra-include-dirs:
  - /usr/local/Cellar/libsass/3.5.5/include/sass/
  - /usr/local/Cellar/libsass/3.5.5/include/
```

- add `extra-lib-dirs`
```yaml
extra-lib-dirs:
  - /usr/local/Cellar/libsass/3.5.5/lib/
```

## add deps to `package.yml`
```yaml
- hakyll == 4.13.*
- hakyll-sass == 0.2.*
- data-default
- hsass  == 0.8.0
- aeson
- bytestring
- string-conversions
- filepath
- MissingH
- binary
- text
- attoparsec
- mtl
- transformers
- errors
- directory
- colour
- pandoc
- cache
- hashable
- raw-strings-qq
- random
- containers
- rainbow == 0.30.0.2
- yaml
```

## setup ruby deps

- `echo "<gemset name>" > .ruby-gemset`
- `echo ruby-2.6.1 > .ruby-version`
- add minimal `Gemfile`:
```ruby
# A sample Gemfile
source "https://rubygems.org"

gem 'sass'
gem 'slim'
gem "pandoc-ruby"
gem 'capistrano'
# gem 'capistrano-ssh-doctor', '~> 1.0'
gem "capistrano-scm-copy"
# gem 'net-ssh' , github: 'net-ssh'#, '3.0.1'
gem 'sshkit'
# gem 'foundation'
# gem 'compass'
# gem "rails"
gem 'guard'
gem 'guard-shell'
gem "guard-process"
gem "guard-rake"
gem "rake"
gem 'rmagick'
```

## create minimal engine
### minimal `src/site.hs`
```haskell
{-# LANGUAGE OverloadedStrings #-}


-- import           Data.ByteString.Lazy as BSL
import           Data.Default (def)
import Data.Maybe (fromMaybe)


-- import           Data.Monoid (mappend, (<>))
import           Hakyll
-- import           Hakyll.Core.Configuration (Configuration, previewPort)
-- import           Hakyll.Core.Metadata
import Hakyll.Core.Compiler (getUnderlying)
import Hakyll.Web.Template.Internal (readTemplate)


import W7W.Types
import W7W.MultiLang
import W7W.Compilers.Slim
import qualified W7W.Cache as Cache
import qualified W7W.Config as W7WConfig

import W7W.Rules.Templates
import W7W.Rules.Assets
import W7W.Pictures.Rules
-- import W7W.Rules.StaticPages

import W7W.Labels.Parser

import Site.Config
import Site.StaticPages.Rules


-- import Site.Context
-- import Site.Template

-- import Site.StaticPages.Rules
-- import Site.Archive.Rules

-- import Site.CollectiveGlossary
-- import Site.CollectiveGlossary.Rules

--------------------------------------------------------------------------------

config :: Configuration
config = W7WConfig.config { previewPort = 8111}

picturesConfig =
  copyAllPicturesRulesConfig {othersStrategy = PicResizeStrategy (1280, 1280)}

main :: IO ()
main = do
  caches <- Cache.newCaches
  labels <- parseLabelsYaml
  cfg <- return $ mkSiteConfig caches labels

  hakyllWith config $
    do

       templatesRules

       imagesRules -- static assets
       picturesRules picturesConfig "pictures"
       fontsRules
       dataRules

       cssAndSassRules ("css/_*.scss" .||. "css/**/_*.scss") [ "css/app.scss"]

       jsRules


       -- slim partials for deps
       -- match ("ru/**/_*.slim" .||. "en/**/_*.slim") $ compile getResourceBody


       staticPagesRules cfg
```

### create `sr/Site/Config.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Site.Config where

import           Data.Default (def)
import qualified Text.Sass.Options as SO
import Hakyll

import W7W.Config

import qualified W7W.Cache as Cache
import W7W.Labels.Types

type Config = (Cache.Caches, Labels)

mkSiteConfig :: Cache.Caches -> Labels -> Config
mkSiteConfig = (,)
```

## create `src/Site/Context.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Site.Context where

import Control.Monad.Trans.Class
import Control.Monad.Reader

import Control.Applicative (Alternative (..))

import System.Random

import Hakyll
import Hakyll.Core.Compiler.Internal (compilerUnsafeIO)

import Data.Monoid ((<>), mempty)

import W7W.MonadCompiler
import W7W.MultiLang
import W7W.Utils
import W7W.Context

import W7W.Labels.Types
import W7W.Labels.Context

import qualified W7W.Cache as Cache


--
-- snapshot "content" of the item
--
fieldContent :: Context String
fieldContent = field "content" content'
  where
    content' i = loadSnapshotBody (itemIdentifier i) "content"


fieldRandomFunction =
  functionField "random" f
  where
    usage = "usage: random(min, max)"
    f [] _ =  error $ "random: empty args. " ++ usage
    f ([_]) _ = error $ "too few args. " ++ usage
    f [minS, maxS] _ = return . show =<< compilerUnsafeIO getRandomInt
      where
        getRandomInt = do
          g <- newStdGen
          (i, g') <- return $ randomR (min, max) g
          return i
        min :: Int
        min = read minS

        max :: Int
        max = read maxS

    f _ _ = error $ "too many args. " ++ usage

--
-- minimal
--
minimalSiteCtx :: Context String
minimalSiteCtx =
  fieldRuUrl
    <> fieldEnUrl
    <> fieldLang
    <> fieldOtherLang
    <> fieldOtherLangUrl
    <> fieldCanonicalName
    <> fieldRandomFunction
    <> defaultContext

--
-- site default
--
mkSiteCtx :: Cache.Caches -> Labels -> Compiler (Context String)
mkSiteCtx caches labels = do
  r <- mkFieldRevision caches
  ls <- runReaderT mkLabelsField labels
  return $ minimalSiteCtx
           <> r
           <> ls

siteCtx :: (MonadReader r m, MonadCompiler m, Cache.HasCache r, HasLabels r) => m (Context String)
siteCtx = do
  c <- asks Cache.getCache
  ls <- asks getLabels
  liftCompiler $ mkSiteCtx c ls
```

### create `src/Site/Templates.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Site.Templates where

import Hakyll

rootTpl :: Identifier
rootTpl = "templates/root.slim"

indexPageTpl :: Identifier
indexPageTpl = "templates/index.slim"
```

### create `src/Site/StaticPages/Rules.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Site.StaticPages.Rules where

import Hakyll

import W7W.Rules.StaticPages

import qualified Site.Config as SiteCfg

import Site.Templates
import Site.StaticPages.Context

staticPagesRules :: SiteCfg.Config -> Rules ()
staticPagesRules cfg = do
  htmlPageRules cfg
  mdPageRules cfg
  slimPageRules cfg


htmlPageRules cfg = do
  staticHtmlPageRulesM rootTpl Nothing Nothing (mkStaticPageCtx cfg) "*.html"

mdPageRules cfg = do
  staticPandocPageRulesM rootTpl Nothing Nothing (mkStaticPageCtx cfg) "*.md"

slimPageRules cfg = do
  staticSlimPageRulesM rootTpl Nothing Nothing (mkStaticPageCtx cfg) "*.slim"
```

### create `src/Site/StaticPages/Context.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Site.StaticPages.Context where

import Hakyll

import qualified Site.Config as SiteCfg
import Site.Context

mkStaticPageCtx :: SiteCfg.Config -> Compiler (Context String)
mkStaticPageCtx cfg = mkSiteCtx (fst cfg) (snd cfg)
```

## build engine

`stack build`

## create minimal website file structure

- `mkdir templates css js ru en images pictures`
- `echo "'$body$" > templates/root.slim` 
- `echo "# hi?" > ru/index.md`
- `touch ./slimlib.rb`
- create minimal labels.yml
```yaml
general:
  someCategory:
    someLabel:
      ru: "ru"
      en: "en"
```

## run hakyll

`stack exec site --  watch --port 8001`

## setup deployment via capistrano

- create `Capfile`
- `mkdir config/deploy`


