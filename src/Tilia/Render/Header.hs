{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The module header, and the module as a whole.
--
-- The header is the one part of a module that is reordered rather than
-- merely re-laid-out: language pragmas are sorted, and a @{-# LANGUAGE A, B
-- #-}@ is split into one pragma per extension. That is safe because the
-- compiler reads the header as a set, with one exception—some extensions
-- turn others on, so the order within a few groups is load-bearing, and
-- 'pragmaOrder' is where that is written down.
module Tilia.Render.Header
  ( -- * Pragmas
    HeaderPragma (..),
    takeHeaderPragmas,

    -- * The module
    hsModule,
    exportList,
    importDecl,
  )
where

import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.Flags (Language)
import GHC.Hs
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Comments (Comment (..), CommentStyle (..), Pragma (..), commentPragma)
import Tilia.Printer.Combinators
import Tilia.Render.Context
import Tilia.Render.Declaration (decls)
import Tilia.Render.Haddock
import Tilia.Render.Layout
import Tilia.Render.Name
import Tilia.Render.Pragma (warningTxt)
import Tilia.Span
import Tilia.Span.Ghc

----------------------------------------------------------------------------
-- Pragmas

-- | A pragma of the file header.
data HeaderPragma = HeaderPragma
  { -- | Comments written directly above it, which travel with it when it is
    -- sorted into place.
    hpComments :: [Comment],
    -- | Where it sorts.
    hpOrder :: PragmaOrder,
    -- | @LANGUAGE@, @OPTIONS_GHC@ or @OPTIONS_HADDOCK@.
    hpName :: Text,
    -- | One extension, or the whole of an options string.
    hpBody :: Text
  }
  deriving (Eq, Show)

-- | Where a pragma sorts among the others.
--
-- The derived ordering is the whole of the policy: language pragmas first,
-- then @OPTIONS_GHC@, then @OPTIONS_HADDOCK@; and within the language
-- pragmas, by the class of extension.
data PragmaOrder
  = LanguageOrder ExtensionClass
  | OptionsGhcOrder
  | OptionsHaddockOrder
  deriving (Eq, Ord, Show)

-- | Which group an extension sorts into.
--
-- Sorting the extensions alphabetically outright would change what a module
-- means, because an extension can turn others on and a later one can turn
-- them off again. Sorting only within these groups keeps the relationships
-- that matter: a pack before what it enables, an enabling before a
-- disabling, and the stragglers that have to come last at the end.
data ExtensionClass
  = -- | @GHC2021@, @Haskell2010@ and the like
    Pack
  | -- | Anything else
    Enabling
  | -- | An extension written with a @No@ prefix
    Disabling
  | -- | Extensions that only work when nothing follows them
    Last'
  deriving (Eq, Ord, Show)

-- | Pick the header pragmas out of a comment stream.
--
-- What comes back is the pragmas, in the order they were written, and the
-- comments that were not pragmas. A @{-# … #-}@ below the header is not a
-- pragma at all—the compiler never reads it—so hoisting it would give it a
-- meaning it did not have, and it is left in the stream as the comment it
-- is.
takeHeaderPragmas ::
  -- | Where the header ends
  Maybe Span ->
  [Comment] ->
  ([HeaderPragma], [Comment])
takeHeaderPragmas headerEnd = go []
  where
    go pending = \case
      [] -> ([], reverse pending)
      (c : cs)
        | Just p <- headerPragma c ->
            let (mine, theirs) = splitLeading c pending
                (ps, rest) = go [] cs
             in (entry (reverse mine) p : ps, reverse theirs <> rest)
        | otherwise -> go (c : pending) cs

    entry comments p =
      HeaderPragma
        { hpComments = comments,
          hpOrder = orderOf p,
          hpName = pragmaName p,
          hpBody = pragmaBody p
        }

    headerPragma c = do
      p <- commentPragma c
      _ <- lookupOrder (pragmaName p)
      if inHeader (commentSpan c) then Just p else Nothing

    inHeader s = case headerEnd of
      Nothing -> True
      Just end -> spanStartLine s < spanStartLine end

    -- Only the comments directly above a pragma travel with it. One
    -- separated by a blank line was written about the file rather than about
    -- the pragma, and moving it would be a guess.
    splitLeading c = climb (spanStartLine (commentSpan c))
      where
        climb line (above : rest)
          | attached line above =
              let (mine, theirs) = climb (spanStartLine (commentSpan above)) rest
               in (above : mine, theirs)
        climb _ pending = ([], pending)

        attached line above =
          not (commentTrailing above)
            && commentStyle above /= DocComment
            && spanEndLine (commentSpan above) + 1 == line

    orderOf p = case pragmaName p of
      "LANGUAGE" -> LanguageOrder (classifyExtension (pragmaBody p))
      other -> maybe OptionsGhcOrder id (lookupOrder other)

    lookupOrder = \case
      "LANGUAGE" -> Just (LanguageOrder Enabling)
      "OPTIONS_GHC" -> Just OptionsGhcOrder
      "OPTIONS_HADDOCK" -> Just OptionsHaddockOrder
      _ -> Nothing

-- | The pragmas of a header, one per line, sorted.
pragmaBlock :: [HeaderPragma] -> Doc
pragmaBlock = foldMap render . sortOn key . nub . concatMap split
  where
    -- Within a group the order is alphabetical, which is the only ordering a
    -- reader can check at a glance.
    key p = (hpOrder p, hpBody p)

    -- @{-# LANGUAGE A, B #-}@ is one pragma written once but two pragmas as
    -- far as the compiler is concerned, and they may sort apart. The comment
    -- above it was written once too, so it goes to the first of them only.
    split p
      | hpName p == "LANGUAGE" = case map T.strip (T.splitOn "," (hpBody p)) of
          [] -> []
          (x : xs) ->
            single (hpComments p) x : map (single []) xs
      | otherwise = [p]
      where
        single comments body =
          p
            { hpComments = comments,
              hpBody = body,
              hpOrder = LanguageOrder (classifyExtension body)
            }

    render p =
      foldMap (\c -> commentLines c <> hardBreak) (hpComments p)
        <> txt "{-# "
        <> txt (hpName p)
        <> space
        <> txt (hpBody p)
        <> txt " #-}"
        <> hardBreak

    -- Printed as plain text rather than as a located comment: the pragma it
    -- belongs to may have been sorted away from where it was written, and a
    -- located node out of order would confuse attachment.
    commentLines c = sepBy (verbatimBreak AtIndent) (map txt (NE.toList (commentBody c)))

-- | Which group an extension belongs to.
classifyExtension :: Text -> ExtensionClass
classifyExtension t
  | t `Set.member` extensionPacks = Pack
  -- @ImplicitPrelude@ and @CUSKs@ are turned off by other extensions, so
  -- asking for either of them only takes effect at the end.
  | t == "ImplicitPrelude" = Last'
  | t == "CUSKs" = Last'
  | otherwise = case T.uncons (T.drop 2 t) of
      Just (c, _) | "No" `T.isPrefixOf` t, c `elem` ['A' .. 'Z'] -> Disabling
      _ -> Enabling

-- | The extension packs, which name a whole edition of the language.
extensionPacks :: Set Text
extensionPacks =
  Set.fromList (map (T.pack . show) [minBound :: Language .. maxBound])

----------------------------------------------------------------------------
-- The module

-- | A whole module.
hsModule :: Ctx -> [HeaderPragma] -> HsModule GhcPs -> Doc
hsModule ctx pragmas HsModule {hsmodExt = XModulePs {..}, ..} =
  layoutFrom ctx headerSpan . brokenIfDocumentedExports exports $
    pragmaBlock pragmas
      <> hardBreak
      <> moduleLine
      <> hardBreak
      <> foldMap (\i -> at_ ctx (importDecl ctx) i <> hardBreak) hsmodImports
      <> hardBreak
      <> layoutFrom ctx (spansOf hsmodDecls) (decls ctx Free hsmodDecls)
  where
    exports = maybe [] unLoc hsmodExports
    headerSpan = foldMap spanOf hsmodDeprecMessage <> foldMap spanOf hsmodExports

    moduleLine = case hsmodName of
      Nothing -> mempty
      Just modName ->
        at ctx modName (\n -> documentation <> moduleHeadName ctx n)
          <> breakOrSpace
          <> foldMap (\w -> at ctx w warningTxt <> breakOrSpace) hsmodDeprecMessage
          <> foldMap exports' hsmodExports
          <> txt "where"
          <> hardBreak

    documentation = foldMap (haddock ctx Pipe Closed) hsmodHaddockModHeader

    exports' l =
      at ctx l (\xs -> indent (exportList ctx (spanOf l) xs)) <> breakOrSpace

----------------------------------------------------------------------------
-- Export lists

-- | The parenthesised list after a module name.
exportList :: Ctx -> Maybe Span -> [LIE GhcPs] -> Doc
exportList ctx enclosing xs =
  brokenIfDocumentedExports xs . parens . insideBrackets enclosing $
    importExportItems ctx xs

-- | The items of an import or export list.
--
-- The comma travels with the item rather than sitting between two of them,
-- because a list that has been broken ends with one: adding an entry then
-- touches one line rather than two.
importExportItems :: Ctx -> [LIE GhcPs] -> Doc
importExportItems ctx xs = variant (laidOut False) (laidOut True)
  where
    laidOut broken' =
      sepBy breakOrSpace [item broken' place x | (place, x) <- places xs]
    item broken' place x =
      sectionGap place (unLoc x)
        <> align (at ctx (widenToDoc x) (ieItem ctx (comma' broken' place)))
    sectionGap place = \case
      IEGroup {} | place == Middle || place == Last -> hardBreak
      _ -> mempty
    comma' broken' place
      | broken' = True
      | otherwise = place == First || place == Middle

-- | Widen an item's span to take in the documentation printed with it, so
-- that a documented item is laid out as one thing.
widenToDoc :: LIE GhcPs -> LIE GhcPs
widenToDoc l@(L ann ie) = case itemDoc ie of
  Nothing -> l
  Just (L docSpan _) -> L (ann <> noAnnSrcSpan docSpan) ie

-- | One item of an import or export list.
ieItem :: Ctx -> Bool -> IE GhcPs -> Doc
ieItem ctx withComma = \case
  IEVar warning n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> comma'
      <> itemDocumentation doc
  IEThingAbs warning n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> comma'
      <> itemDocumentation doc
  IEThingAll (warning, _) n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> space
      <> txt "(..)"
      <> comma'
      <> itemDocumentation doc
  IEThingWith (warning, _) n wildcard members doc ->
    align
      ( exportWarning warning
          <> at ctx n (wrappedName ctx)
          <> breakOrSpace
          <> indent (parens (commaSep (align <$> withWildcard)))
          <> comma'
      )
      <> itemDocumentation doc
    where
      rendered = map (at_ ctx (wrappedName ctx)) members
      withWildcard = case wildcard of
        NoIEWildcard -> rendered
        IEWildcard n' ->
          let (before, after) = splitAt n' rendered
           in before <> [txt ".."] <> after
  IEModuleContents (warning, _) m ->
    exportWarning warning <> at ctx m (moduleHeadName ctx) <> comma'
  IEGroup NoExtField n str -> haddock ctx (Section n) Open str
  IEDoc NoExtField str -> haddock ctx Pipe Open str
  IEDocNamed NoExtField n -> txt (docSectionName n)
  where
    comma' = includeWhen withComma comma
    exportWarning =
      foldMap (\w -> at ctx w warningTxt <> breakOrSpace)
    itemDocumentation =
      foldMap (\d -> breakOrSpace <> haddock ctx Caret Open d)

itemDoc :: IE GhcPs -> Maybe (ExportDoc GhcPs)
itemDoc = \case
  IEVar _ _ doc -> doc
  IEThingAbs _ _ doc -> doc
  IEThingAll _ _ doc -> doc
  IEThingWith _ _ _ _ doc -> doc
  _ -> Nothing

-- | A list holding a documentation entry cannot go on one line: the entry
-- would swallow the rest of it, closing bracket and all.
brokenIfDocumentedExports :: [LIE GhcPs] -> Doc -> Doc
brokenIfDocumentedExports xs
  | any (isDocEntry . unLoc) xs = broken
  | otherwise = id
  where
    isDocEntry = \case
      IEDoc {} -> True
      IEGroup {} -> True
      IEDocNamed {} -> True
      _ -> False

----------------------------------------------------------------------------
-- Imports

-- | One import declaration.
importDecl :: Ctx -> ImportDecl GhcPs -> Doc
importDecl ctx ImportDecl {..} =
  txt "import"
    <> space
    <> includeWhen (ideclSource == IsBoot) (txt "{-# SOURCE #-}")
    <> space
    <> includeWhen ideclSafe (txt "safe")
    <> space
    <> levelBefore
    <> space
    <> includeWhen (isQualified && not qualifiedLast) (txt "qualified")
    <> space
    <> packageQualifier
    <> space
    <> indent
      ( at ctx ideclName outputable
          <> space
          <> levelAfter
          <> includeWhen (isQualified && qualifiedLast) (space <> txt "qualified")
          <> foldMap (\a -> space <> txt "as" <> space <> at ctx a outputable) ideclAs
          <> space
          <> importList
      )
  where
    -- Which side of the module name @qualified@ goes is left as the author
    -- had it. Normalising would mean consulting @ImportQualifiedPost@, and
    -- an edition of the language turns that on without anyone writing it
    -- down, so the extension is not reliably knowable from the file. The
    -- author's own choice is, and it is recorded right here.
    qualifiedLast = ideclQualified == QualifiedPost
    isQualified = isImportDeclQualified ideclQualified

    packageQualifier = case ideclPkgQual of
      NoRawPkgQual -> mempty
      RawPkgQual literal -> outputable literal

    levelBefore = case ideclLevelSpec of
      LevelStylePre l -> declLevel l
      _ -> mempty
    levelAfter = case ideclLevelSpec of
      LevelStylePost l -> declLevel l
      _ -> mempty

    importList = case ideclImportList of
      Nothing -> mempty
      Just (interpretation, L listLoc xs) ->
        hidden
          <> breakOrSpace
          <> parens
            ( insideBrackets
                (spanOfSrcSpan (locA listLoc))
                (importExportItems ctx xs)
            )
        where
          hidden = case interpretation of
            Exactly -> mempty
            EverythingBut -> txt "hiding"

declLevel :: ImportDeclLevel -> Doc
declLevel = \case
  ImportDeclSplice -> txt "splice"
  ImportDeclQuote -> txt "quote"
