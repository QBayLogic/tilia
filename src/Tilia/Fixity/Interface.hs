{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a module's operators out of interface files.
module Tilia.Fixity.Interface
  ( Interface (..),
    readInterface,
    parseInterface,
  )
where

import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Tilia.Fixity
import Tilia.Utils (quietly)

-- | What an interface says about the operators a module offers.
data Interface = Interface
  { -- | The fixities the module declares itself.
    interfaceDeclares :: Map OpName Fixity,
    -- | The names it exports that some other module declared, each with the
    -- module that did. Not only the operators: a plain function can be
    -- given a fixity and used in backticks, and one of these is where the
    -- declaration would be.
    interfacePassedOn :: [(Text, OpName)]
  }
  deriving (Eq, Show)

-- | Read a module's interface file, if the compiler will show it to us.
--
-- 'Nothing' where it will not, which covers a file that is not there, one
-- built by another compiler, and @ghc@ not being on the path at all. None
-- of those is fatal; they only mean this module has nothing to add.
readInterface ::
  -- | The module the file is supposed to hold
  Text ->
  -- | The file
  FilePath ->
  IO (Maybe Interface)
readInterface modName path = quietly Nothing $ do
  (code, out, _) <- readProcessWithExitCode "ghc" ["--show-iface", path] ""
  pure $ case code of
    ExitSuccess -> parseInterface modName (T.pack out)
    _ -> Nothing

-- | Read what @ghc --show-iface@ printed, if it is this module's interface.
parseInterface :: Text -> Text -> Maybe Interface
parseInterface modName out
  | not (any holdsModule (T.lines out)) = Nothing
  | otherwise =
      Just
        Interface
          { interfaceDeclares = Map.fromList (concatMap declared (sectionsNamed "fixities")),
            interfacePassedOn = concatMap passedOn (sectionsNamed "exports:")
          }
  where
    holdsModule l = case T.words l of
      ("interface" : m : _) -> m == modName
      _ -> False
    sectionsNamed name = [body | (heading, body) <- sections out, heading == name]
    declared = mapMaybe fixityEntry . T.splitOn ","
    passedOn = concatMap fromExport . T.words

-- | Split the output into sections.
sections :: Text -> [(Text, Text)]
sections = go . T.lines
  where
    go = \case
      [] -> []
      (l : ls)
        | indented l -> go ls
        | otherwise ->
            let (body, rest) = span indented ls
                (heading, opening) = T.breakOn " " l
             in (heading, T.unwords (opening : body)) : go rest
    indented l = maybe False (== ' ') (fst <$> T.uncons l)

-- | One entry of a @fixities@ line: @infixl 9 !@ and the like.
fixityEntry :: Text -> Maybe (OpName, Fixity)
fixityEntry entry = case T.words entry of
  [direction, precedence, op] -> do
    d <- case direction of
      "infixl" -> Just LeftAssoc
      "infixr" -> Just RightAssoc
      "infix" -> Just NoAssoc
      _ -> Nothing
    p <- readPrecedence precedence
    pure (OpName op, Fixity d p)
  _ -> Nothing
  where
    -- Not one digit: GHC gives @->@ a precedence of -1, below anything the
    -- report allows anyone to write, and drops it into a fixities line like
    -- any other.
    readPrecedence t = case T.signed T.decimal t of
      Right (p, rest) | T.null rest -> Just p
      _ -> Nothing

-- | The names an export entry passes on, with the module that declared each.
--
-- An entry is a name, and a type or class is followed by its members in
-- braces. A name written bare was declared by the module whose interface
-- this is, and is left out: its fixity is in the @fixities@ line already.
fromExport :: Text -> [(Text, OpName)]
fromExport = mapMaybe qualified . T.split (`elem` ("{}|," :: String))
  where
    qualified name = case moduleOf name of
      Just (m, n) | not (T.null n) -> Just (m, OpName n)
      _ -> Nothing

-- | Split a name into the module that declared it and the name itself.
moduleOf :: Text -> Maybe (Text, Text)
moduleOf = go []
  where
    go seen t = case component t of
      Just (c, rest) -> go (c : seen) rest
      Nothing
        | null seen -> Nothing
        | otherwise -> Just (T.intercalate "." (reverse seen), t)
    component t = do
      (c, _) <- T.uncons t
      if isUpper c
        then case T.break (== '.') t of
          (before, rest)
            | Just after <- T.stripPrefix "." rest, not (T.null before) ->
                Just (before, after)
          _ -> Nothing
        else Nothing
