-- | Turning a printed document into source text.
module Tilia.Printer
  ( -- * Documents
    Doc,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    printDoc,
  )
where

import Data.Text (Text)
import Tilia.Printer.Internal
  ( Doc,
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )

-- | Render a document to source text.
printDoc :: RenderOptions -> Doc -> Text
printDoc = render
