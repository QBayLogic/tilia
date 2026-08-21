-- | Turning a printed document into source text.
module Linden.Printer
  ( -- * Documents
    Doc,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    printDoc,
  )
where

import Data.Text (Text)
import Linden.Printer.Internal
  ( Doc,
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )

-- | Render a document to source text.
printDoc :: RenderOptions -> Doc -> Text
printDoc = render
