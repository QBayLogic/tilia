-- | Turning a printed document into source text.
module Tilia.Doc
  ( -- * Documents
    Doc,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    printDoc,
  )
where

import Data.Text (Text)
import Tilia.Doc.Internal
  ( Doc,
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )

-- | Render a document to source text.
printDoc :: RenderOptions -> Doc -> Text
printDoc = render
