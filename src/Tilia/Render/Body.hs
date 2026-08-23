{-# LANGUAGE LambdaCase #-}

-- | Which constructs absorb the line break that introduces them.
--
-- A body is a node together with the site it stands at, and the one question
-- an enclosing construct has to ask of it is where to put it: on the line it
-- has already started, or on the next one indented. "Tilia.Doc.Body"
-- states that question as a class; this module answers it, for the two kinds
-- of node that can stand as a body.
--
-- The answers are a table, not an argument. Threading a @body -> Placement@
-- callback through every construct that has a body—equations, guards, @if@,
-- @let@, @case@, lambdas, statements—spreads one small piece of knowledge
-- across a dozen signatures and makes each of them carry a second parameter
-- that only ever has two possible values. Here it is written down once, and
-- what the constructs pass around is the body itself.
module Tilia.Render.Body
  ( -- * Bodies
    ExprBody (..),
    CmdBody (..),
    CmdTopBody (..),

    -- * The table
    exprHangs,
    cmdHangs,
    cmdTopHangs,
  )
where

import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Doc.Body
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Span
import Tilia.Span.Ghc

----------------------------------------------------------------------------
-- Bodies

-- | An expression standing as the body of an enclosing construct.
data ExprBody = ExprBody Ctx Site (LHsExpr GhcPs)

instance Body ExprBody where
  printBody (ExprBody ctx site e) = knotExpr (ctxKnot ctx) ctx site e
  bodyPlacement (ExprBody _ _ e) = exprHangs (unLoc e)

-- | A command standing as the body of an enclosing construct.
data CmdBody = CmdBody Ctx Site (LHsCmd GhcPs)

instance Body CmdBody where
  printBody (CmdBody ctx site c) = knotCmd (ctxKnot ctx) ctx site c
  bodyPlacement (CmdBody _ _ c) = cmdHangs (unLoc c)

-- | A command at the top of an arrow form.
data CmdTopBody = CmdTopBody Ctx Site (LHsCmdTop GhcPs)

instance Body CmdTopBody where
  printBody (CmdTopBody ctx site l) =
    at ctx l (\(HsCmdTop _ cmd) -> knotCmd (ctxKnot ctx) ctx site cmd)
  bodyPlacement (CmdTopBody _ _ l) = cmdTopHangs (unLoc l)

----------------------------------------------------------------------------
-- The table

-- | Does this expression absorb the line break that introduces it?
--
-- A @do@ block, a @case@ and a lambda all begin with a keyword and continue
-- on the lines below, so @f = do@ costs nothing and saves a line. Everything
-- not named here has to start on a line of its own.
exprHangs :: HsExpr GhcPs -> Placement
exprHangs = \case
  HsDo _ (DoExpr _) _ -> Hanging
  HsDo _ (MDoExpr _) _ -> Hanging
  HsCase {} -> Hanging
  HsLam _ lamVariant mg -> case lamVariant of
    LamCase -> Hanging
    LamCases -> Hanging
    -- A lambda whose parameters ran over several lines leaves its body
    -- indented under nothing legible, so only a compact one hangs.
    LamSingle -> case mg of
      MG _ (L _ [L _ (Match _ _ (L _ ps@(_ : _)) _)])
        | maybe False isSingleLine (spansOf ps) -> Hanging
      _ -> Normal
  HsProc _ p _
    -- The indentation breaks when the pattern runs over more than one line,
    -- so hanging is only safe when it does not.
    | maybe False isSingleLine (spanOf p) -> Hanging
    | otherwise -> Normal
  -- An application hangs on its last argument, and a chain through @$@ on
  -- its right operand: both of those are the thing that would be introduced.
  -- No other operator qualifies, @$@ being the one whose whole purpose is to
  -- hand a block to what precedes it.
  HsApp _ _ y -> exprHangs (unLoc y)
  OpApp _ _ op y
    | Just n <- operatorName op,
      occNameString (rdrNameOcc n) == "$" ->
        exprHangs (unLoc y)
  _ -> Normal

-- | Does this command absorb the line break that introduces it?
cmdHangs :: HsCmd GhcPs -> Placement
cmdHangs = \case
  HsCmdDo {} -> Hanging
  HsCmdCase {} -> Hanging
  HsCmdLam {} -> Hanging
  _ -> Normal

cmdTopHangs :: HsCmdTop GhcPs -> Placement
cmdTopHangs (HsCmdTop _ c) = cmdHangs (unLoc c)

-- | The name of an operator, when the expression standing as one is a name.
operatorName :: LHsExpr GhcPs -> Maybe RdrName
operatorName e = case unLoc e of
  HsVar _ (L _ n) -> Just n
  _ -> Nothing
