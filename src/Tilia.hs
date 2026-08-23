-- | A formatter for Haskell source code. This module exposes the official
-- stable API; other modules may not be as reliable.
module Tilia
  ( format,
    ParseError,
    describeParseError,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import Tilia.Parser
  ( ParseError,
    ParsedModule,
    defaultParserConfig,
    describeParseError,
    parseText,
    effectiveExtensions,
  )
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Render (Settings (..), defaultSettings, renderModule)

-- | Format a Haskell module, reporting where it failed to parse.
format :: Text -> Either ParseError Text
format source = render <$> parseText defaultParserConfig "<input>" source
  where
    render :: ParsedModule -> Text
    render parsed = printDoc defaultRenderOptions (renderModule settings parsed)
    settings =
      defaultSettings
        { setExtensions = Set.fromList (effectiveExtensions source)
        }
