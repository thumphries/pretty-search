{- | Just messing with LogicT, no cursor or nesting -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Logic0 where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Logic (LogicT (..))
import qualified Control.Monad.Logic as LogicT
import           Control.Monad.Trans.Class (MonadTrans (..))

import           Data.Foldable (maximum)
import           Data.Function (on)
import           Data.Functor.Identity (Identity (..))
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Ord (Down (..))
import           Data.Semigroup (Semigroup (..))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T


{--

TODO loose plans

create a Doc tracking line/col/indent level
(independent of underlying text type)

ReaderT of cursor, heuristics, guards

express Ribbon as an heuristic and a guard (soft max/hard max)

Apply heuristics and guards... somewhere.
maybe on each bind, or in the Alternative instance, or...
(this is hard and will determine the effectiveness)

top level: Guards and Heuristics are late-bound

extend with annotations

--}

newtype SearchT m a = SearchT {
    unSearchT :: LogicT m a
  } deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

instance (Applicative f, Semigroup a) => Semigroup (SearchT f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (SearchT f a) where
  mappend = liftA2 mappend
  mempty = pure mempty

type Search = SearchT Identity

runSearch :: Search doc -> doc
runSearch =
  runIdentity . runSearchT

runSearchT :: Monad m => SearchT m doc -> m doc
runSearchT =
  LogicT.observeT . unSearchT

permuteT :: Monad m => SearchT m doc -> m [doc]
permuteT =
  LogicT.observeAllT . unSearchT

-- | Search depth.
newtype Depth = Depth {
    unDepth :: Int
  } deriving (Eq, Ord, Show, Enum, Integral, Real, Num)

defaultDepth :: Depth
defaultDepth =
  Depth 1000

-- -----------------------------------------------------------------------------

-- | Things that guide the search, without affecting its depth.
newtype Heuristic m a = Heuristic {
    runHeuristic :: SearchT m a -> SearchT m a
  }

choose :: Monad m => (NonEmpty a -> m a) -> Heuristic m a
choose =
  chooseDepth defaultDepth

chooseDepth :: Monad m => Depth -> (NonEmpty a -> m a) -> Heuristic m a
chooseDepth depth f =
  Heuristic $ \(SearchT p) -> SearchT $ do
    rs <- lift $ LogicT.observeManyT (unDepth depth) p
    case rs of
      [] ->
        empty
      (x:xs) ->
        lift $ f (x :| xs)

chooseBy :: Monad m => (a -> a -> Ordering) -> Heuristic m a
chooseBy =
  chooseByDepth defaultDepth

chooseOn :: (Monad m, Ord b) => (a -> b) -> Heuristic m a
chooseOn =
  chooseOnDepth defaultDepth

chooseByDepth :: Monad m => Depth -> (a -> a -> Ordering) -> Heuristic m a
chooseByDepth depth ordering =
  Heuristic $ \(SearchT p) -> SearchT $ do
    rs <- lift $ LogicT.observeManyT (unDepth depth) p
    case rs of
      [] ->
        empty
      xs ->
        -- Hmm, this may ruin backtracking?
        -- Try: asum (fmap (sortBy ordering xs))
        return $ L.maximumBy ordering xs

chooseOnDepth :: (Monad m, Ord b) => Depth -> (a -> b) -> Heuristic m a
chooseOnDepth depth f =
  chooseByDepth depth (compare `on` f)

-- -----------------------------------------------------------------------------

-- | Things that restrict the search.
newtype Guard m a = Guard {
    runGuard :: SearchT m a -> SearchT m a
  }

guardOn :: Monad m => (a -> m Bool) -> Guard m a
guardOn predi =
  Guard $ \(SearchT p) -> SearchT $ do
    a <- p
    b <- lift (predi a)
    guard b
    return a

-- -----------------------------------------------------------------------------

string :: (Monad m, IsString s) => s -> SearchT m s
string =
  pure

chooseTallest :: Monad m => Heuristic m Text
chooseTallest =
  chooseOn height

chooseShortest :: Monad m => Heuristic m Text
chooseShortest =
  chooseBy (compare `on` (Down . height))

chooseWidest :: Monad m => Heuristic m Text
chooseWidest =
  chooseOn maxWidth

chooseNarrowest :: Monad m => Heuristic m Text
chooseNarrowest =
  chooseBy (compare `on` (Down . maxWidth))

maxWidth :: Text -> Int
maxWidth =
  maximum . fmap T.length . T.lines

height :: Text -> Int
height =
  length . T.lines

-- -----------------------------------------------------------------------------

testPrinter :: Search Text
testPrinter =
      string "foobarbaz"
  <|> string "foo\nbar\nbaz"

testPrinter2 :: Search Text
testPrinter2 =
     runHeuristic chooseTallest testPrinter
  <> runHeuristic chooseWidest testPrinter

tallestTest :: Search Text
tallestTest =
  runHeuristic chooseTallest testPrinter

shortestTest :: Search Text
shortestTest =
  runHeuristic chooseShortest testPrinter

widestTest :: Search Text
widestTest =
  runHeuristic chooseWidest testPrinter

narrowestTest :: Search Text
narrowestTest =
  runHeuristic chooseNarrowest testPrinter
