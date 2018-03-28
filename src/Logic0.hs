{- | Just messing with LogicT, no cursor or nesting -}
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
import           Data.Ord (Down (..))
import           Data.Semigroup (Semigroup (..))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T


newtype PrettyT m a = PrettyT {
    unPrettyT :: LogicT m a
  } deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans)

instance (Applicative f, Semigroup a) => Semigroup (PrettyT f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (PrettyT f a) where
  mappend = liftA2 mappend
  mempty = pure mempty

type Pretty = PrettyT Identity

runPretty :: Pretty doc -> doc
runPretty =
  runIdentity . runPrettyT

runPrettyT :: Monad m => PrettyT m doc -> m doc
runPrettyT =
  LogicT.observeT . unPrettyT

permuteT :: Monad m => PrettyT m doc -> m [doc]
permuteT =
  LogicT.observeAllT . unPrettyT

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
    runHeuristic :: PrettyT m a -> PrettyT m a
  }

chooseBy :: Monad m => (a -> a -> Ordering) -> PrettyT m a -> PrettyT m a
chooseBy =
  chooseByDepth defaultDepth

chooseOn :: (Monad m, Ord b) => (a -> b) -> PrettyT m a -> PrettyT m a
chooseOn =
  chooseOnDepth defaultDepth

chooseByDepth :: Monad m => Depth -> (a -> a -> Ordering) -> PrettyT m a -> PrettyT m a
chooseByDepth depth ordering (PrettyT p) =
  PrettyT $ do
    rs <- lift $ LogicT.observeManyT (unDepth depth) p
    case rs of
      [] ->
        empty
      xs ->
        return $ L.maximumBy ordering xs

chooseOnDepth :: (Monad m, Ord b) => Depth -> (a -> b) -> PrettyT m a -> PrettyT m a
chooseOnDepth depth f =
  chooseByDepth depth (compare `on` f)

-- -----------------------------------------------------------------------------

-- | Things that restrict the search.
newtype Guard m a = Guard {
    runGuard :: PrettyT m a -> PrettyT m a
  }

-- -----------------------------------------------------------------------------

string :: (Monad m, IsString s) => s -> PrettyT m s
string =
  pure

chooseTallest :: Monad m => PrettyT m Text -> PrettyT m Text
chooseTallest =
  chooseOn height

chooseShortest :: Monad m => PrettyT m Text -> PrettyT m Text
chooseShortest =
  chooseBy (compare `on` (Down . height))

chooseWidest :: Monad m => PrettyT m Text -> PrettyT m Text
chooseWidest =
  chooseOn maxWidth

chooseNarrowest :: Monad m => PrettyT m Text -> PrettyT m Text
chooseNarrowest =
  chooseBy (compare `on` (Down . maxWidth))

maxWidth :: Text -> Int
maxWidth =
  maximum . fmap T.length . T.lines

height :: Text -> Int
height =
  length . T.lines

-- -----------------------------------------------------------------------------

testPrinter :: Pretty Text
testPrinter =
      string "foobarbaz"
  <|> string "foo\nbar\nbaz"

testPrinter2 :: Pretty Text
testPrinter2 =
     chooseTallest testPrinter
  <> chooseWidest testPrinter

tallestTest :: Pretty Text
tallestTest =
  chooseTallest testPrinter

shortestTest :: Pretty Text
shortestTest =
  chooseShortest testPrinter

widestTest :: Pretty Text
widestTest =
  chooseWidest testPrinter

narrowestTest :: Pretty Text
narrowestTest =
  chooseNarrowest testPrinter
