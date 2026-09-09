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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Tilia.Fixity
import Tilia.Utils (quietly)

-- | What an interface says about the operators a module offers.
data Interface = Interface
  { -- | The fixities the module declares itself, by the namespace each
    -- governs.
    interfaceDeclares :: Fixities,
    -- | The names it exports that some other module declared, each with the
    -- module that did. Not only the operators: a plain function can be
    -- given a fixity and used in backticks, and one of these is where the
    -- declaration would be.
    interfacePassedOn :: [(Text, OpName)],
    -- | What it exports under each name, for the names that carry others
    -- with them. This is what @T(..)@ in an import list stands for, and the
    -- compiler has already worked it out: an export entry wears its members
    -- in braces.
    interfaceChildren :: Map OpName (Set OpName)
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
          { interfaceDeclares =
              namespaced
                (typeNamesIn out)
                (Map.fromList (concatMap declared (sectionsNamed "fixities"))),
            interfacePassedOn = concatMap passedOn (sectionsNamed "exports:"),
            interfaceChildren =
              Map.unionsWith Set.union (map childrenIn (sectionsNamed "exports:"))
          }
  where
    holdsModule l = case T.words l of
      ("interface" : m : _) -> m == modName
      _ -> False
    sectionsNamed name = [body | (heading, body) <- sections out, heading == name]
    declared = mapMaybe fixityEntry . T.splitOn ","
    passedOn = concatMap fromExport . T.words
    childrenIn section =
      Map.fromListWith
        Set.union
        [ (nameOnly parent, Set.fromList (map nameOnly kids))
        | (parent, kids@(_ : _)) <- exportEntries section
        ]

-- | The names an interface declares as types.
--
-- The compiler writes each declaration out, and a type is written as one:
-- @data (:~:) a b where@, @type (==) :: …@, @class Eq a where@. A name
-- that turns up in none of those is a value, which is the other namespace.
typeNamesIn :: Text -> Set OpName
typeNamesIn out =
  Set.fromList
    [ nameOnly (T.dropWhileEnd (== ')') (T.dropWhile (== '(') name))
    | l <- T.lines out,
      indented l,
      (keyword : rest) <- [T.words l],
      keyword `elem` (["data", "type", "newtype", "class"] :: [Text]),
      name <- take 1 (dropWhile (`elem` (["family", "role", "instance"] :: [Text])) rest)
    ]
  where
    indented l = maybe False (== ' ') (fst <$> T.uncons l)

-- | Sort declared fixities into the namespaces they govern.
--
-- A fixity for a name the interface declares as a type governs types; one
-- for any other name governs terms. A name that is both—rare, and legal—
-- gets the fixity in both, which is what the interface says: it records
-- one fixity for the name and no namespace of its own.
namespaced :: Set OpName -> Map OpName Fixity -> Fixities
namespaced types declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixity) <- Map.toList declared,
      namespace <- if Set.member op types then [InTypes] else [InTerms]
    ]

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

-- | Split an exports section into its entries, keeping the members an entry
-- wears in braces with the name they belong to.
--
-- An entry is @Some.Module.T@, or @Some.Module.T{Some.Module.A
-- Some.Module.B}@ where @T@ carries names with it. A partial export writes
-- the name as @T|@, which says that not all of them are there; the ones in
-- the braces are still exactly what @T(..)@ would bring in.
exportEntries :: Text -> [(Text, [Text])]
exportEntries = go
  where
    go text = case T.uncons (T.dropWhile (== ' ') text) of
      Nothing -> []
      Just _ ->
        let trimmed = T.dropWhile (== ' ') text
            (name, rest) = T.break (\c -> c == ' ' || c == '{') trimmed
         in case T.uncons rest of
              Just ('{', inside) ->
                let (kids, after) = T.break (== '}') inside
                 in (bare name, T.words kids) : go (T.drop 1 after)
              _ -> (bare name, []) : go rest
    bare = T.dropWhileEnd (`elem` ("|," :: String))

-- | An exported name without the module that declared it.
nameOnly :: Text -> OpName
nameOnly t = OpName (maybe t snd (moduleOf t))

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
            | Just after <- T.stripPrefix "." rest,
              not (T.null before) ->
                Just (before, after)
          _ -> Nothing
        else Nothing
