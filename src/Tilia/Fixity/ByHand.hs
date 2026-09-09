{-# LANGUAGE OverloadedStrings #-}

-- | Fixities for the few modules whose source defeats us.
module Tilia.Fixity.ByHand
  ( byHandFixities,
    hscFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | The modules, and the operators they declare.
byHandFixities :: Map Text (Map OpName Fixity)
byHandFixities =
  Map.fromList
    [ -- @Test.QuickCheck.Property@ invokes a macro it defines:
      -- @WITNESSES(:: [Witness])@ stands in the middle of a record, and
      -- only @cpp@ can expand it. No configuration of the module is
      -- Haskell, so there is nothing to parse in any of them.
      entry
        "Test.QuickCheck.Property"
        [ ("==>", RightAssoc, 0),
          (".&.", RightAssoc, 1),
          (".&&.", RightAssoc, 1),
          (".||.", RightAssoc, 1),
          ("===", NoAssoc, 4),
          ("=/=", NoAssoc, 4)
        ],
      -- @network@ writes its system calls as @foreign import CALLCONV@,
      -- and @CALLCONV@ is a macro out of @HsNetDef.h@ standing where the
      -- calling convention goes. It has to be expanded for the line to be
      -- Haskell at all, so no configuration of these five parses.
      --
      -- Not one of them declares a fixity, defines an operator, or gives a
      -- constructor an operator name, in any version; every entry below is
      -- therefore empty. What that buys is not their own operators but
      -- everything downstream: @Network.Socket@ is perfectly readable and
      -- was only ever refused because these are what it passes on, and
      -- refusing it refused @wai@, @http-client@, @warp@ and in the end
      -- every @servant@ module that reaches one of them.
      entry "Network.Socket.If" [],
      entry "Network.Socket.Internal" [],
      entry "Network.Socket.Name" [],
      entry "Network.Socket.Shutdown" [],
      entry "Network.Socket.Syscall" [],
      -- @Data.HashMap.Internal.Array@ defines @CHECK_BOUNDS@ and calls it
      -- where an expression goes, with the guarded @case@ on the line
      -- below. Unexpanded it reads as a function applied to that @case@,
      -- and both branches of the @#if@ that defines it leave the call
      -- standing, so there is no configuration to fall back on.
      --
      -- It exports no operator and declares no fixity. What it costs to
      -- refuse is @Data.HashMap.Strict@, and after that @Data.Aeson.KeyMap@
      -- and everything that reads a JSON object.
      entry "Data.HashMap.Internal.Array" []
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])

-- | What the modules written for @hsc2hs@ declare.
--
-- A different question from 'byHandFixities', and asked at a different
-- moment. That table is a last resort for a module nothing could be made
-- of; this one is the whole answer for an @.hsc@, given instead of reading
-- it, and a module absent from here declares nothing rather than being
-- unreadable.
--
-- The claim behind the absence is that @hsc2hs@ modules hardly ever declare
-- a fixity. Of three hundred @.hsc@ files across the packages this project
-- builds against, exactly one module declares one, and it is below. One
-- other defines operators at all — @regex-posix@'s @Text.Regex.Posix.Wrap@,
-- which gives @=~@ and @=~~@ and no fixity for either, so the Report's
-- @infixl 9@ is what they have and is what an absence here already says.
-- Neither module can be parsed even with the @hsc2hs@ constructs blanked
-- out, so there was never a reading that would have found them.
--
-- Being wrong here costs indentation and nothing else: a fixity decides
-- how a chain of operators is grouped when it is broken across lines, and
-- nothing in the renderer adds or removes a parenthesis. A module missing
-- from this table is laid out as though its operators were @infixl 9@.
hscFixities :: Map Text (Map OpName Fixity)
hscFixities =
  Map.fromList
    [ -- @addSignal@ and @deleteSignal@ take a signal on the left and a set
      -- on the right, so a chain of them only typechecks to the right, and
      -- the module says so with a bare @infixr@—precedence 9, as the
      -- Report has it when none is written.
      entry
        "System.Posix.Signals"
        [ ("addSignal", RightAssoc, 9),
          ("deleteSignal", RightAssoc, 9)
        ]
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])
