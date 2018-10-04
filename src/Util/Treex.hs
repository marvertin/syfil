{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Util.Treex (

 Tree(..),
 zipPath,
 intersection,
 buildDownM,
 fillNodes

) where

import           Data.Functor.Identity
import           Data.Kind
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Set              as S


data Tree k v = Tree v (M.Map k (Tree k v)) deriving Show

instance Functor (Tree k) where
  fmap fce (Tree x ch) = Tree (fce x) (M.map (fmap fce) ch)

zipPath :: Tree k v -> Tree k ([k], v)
zipPath = zp []
  where
    zp :: [k] -> Tree k v -> Tree k ([k], v)
    zp revpath (Tree x ch) =
         Tree (revpath, x) (M.mapWithKey (\k tree -> zp (k:revpath) tree) ch)


instance Foldable (Tree k) where
  foldMap fce (Tree x ch) = fce x <> foldMap (foldMap fce) ch

instance Traversable (Tree k) where
  traverse fce (Tree x ch) = Tree <$> fce x <*> traverse (traverse fce) ch

instance (Monoid a, Ord k) => Monoid (Tree k a) where
  mempty = Tree mempty M.empty
  (Tree x xch) `mappend` (Tree y ych) = Tree (x <> y) (M.unionWith mappend xch ych)

class RMonad m where
  type RMonadCtxt m a :: Constraint
  type RMonadCtxt m a = ()

  returnx :: RMonadCtxt m a => a -> m a
  (>>==) :: (RMonadCtxt m a, RMonadCtxt m b) => m a -> (a -> m b) -> m b

instance Ord k => RMonad (Tree k) where
  type RMonadCtxt (Tree k) a = Monoid a
  returnx = singleton []
  Tree x ch >>== f = f x <> Tree mempty (M.map (>>== f) ch)
{-
instance (Ord k) => MonadPlus (Tree k) where
  returnx x = Tree x M.empty
  Tree x ch >>== f = f x <> M.map (>>= f) ch
-}

unionTuples :: Ord k => Tree k a -> Tree k b -> Tree k (Maybe a, Maybe b)
unionTuples treeA treeB = uni (fmap (\a -> (Just a, Nothing)) treeA)
                        (fmap (\b -> (Nothing, Just b)) treeB)
  where
    uni :: Ord k => Tree k (Maybe a, Maybe b) -> Tree k (Maybe a, Maybe b) -> Tree k (Maybe a, Maybe b)
    uni (Tree (x, _) xch) (Tree (_, y) ych) = Tree (x, y) (M.unionWith uni xch ych)

unionMap :: (Ord k, Monoid m) => (a -> m) -> (b -> m) -> Tree k a -> Tree k b -> Tree k m
unionMap fa fb treeA treeB = fmap fa treeA <> fmap fb treeB

intersection :: Ord k => Tree k a -> Tree k b -> Tree k (a, b)
intersection (Tree x xch) (Tree y ych) = Tree (x, y) (M.intersectionWith intersection xch ych)


buildx :: Ord k => ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
buildx = bu []
  where
   bu ::  Ord k => [k] -> ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
   bu path fd fn = case fd path of
     Left keys -> let ch = M.fromList (fmap (\k -> (k, bu (k:path) fd fn) ) keys)
                   in Tree (fn path ch) ch
     Right a -> Tree a M.empty

build :: Ord k => ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
build = bu []
 where
  bu ::  Ord k => [k] -> ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
  bu path fd fn = case fd path of
    Left keys -> let ch = M.fromList (fmap (\k -> (k, bu (k:path) fd fn) ) (S.elems keys))
                  in Tree (fn path ch) ch
    Right a -> Tree a M.empty

build2 :: Ord k => ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
build2 fd fn = runIdentity $ buildM (return . fd) (\x y -> return (fn x y))

buildDownM :: (Monad m, Ord k) => ([k] -> m (Either (S.Set k) a)) -> m (Tree k (Maybe a))
buildDownM = bu []
 where
  bu ::   (Monad m, Ord k) => [k] -> ([k] -> m (Either (S.Set k) a)) -> m (Tree k (Maybe a))
  bu path fd = fd path >>= (\node -> case node of
    Left keys -> let ch = M.fromList <$> traverse (\k -> sequence (k, bu (k:path) fd) ) (S.elems keys)
                  in Tree Nothing <$> ch
    Right a ->  return $ Tree (Just a) M.empty)

fillNodes :: Ord k =>  (M.Map k (Tree k a) -> a) -> Tree k (Maybe a) -> Tree k a
fillNodes fce (Tree may ch) = let newCh = M.map (fillNodes fce) ch
                              in case may of
                                 Just x  -> Tree x newCh
                                 Nothing -> Tree (fce newCh) newCh

buildM :: (Monad m, Ord k) => ([k] -> m (Either (S.Set k) a)) -> ([k] -> M.Map k (Tree k a) -> m a) -> m (Tree k a)
buildM = bu []
 where
  bu ::   (Monad m, Ord k) => [k] -> ([k] -> m (Either (S.Set k) a)) -> ([k] -> M.Map k (Tree k a) -> m a) -> m (Tree k a)
  bu path fd fn = fd path >>= (\node -> case node of
    Left keys -> let ch = M.fromList <$> (traverse (\k -> sequence (k, bu (k:path) fd fn) ) (S.elems keys))
                  in Tree <$> (fn path =<< ch) <*> ch
    Right a ->  return $ Tree a M.empty)

lookupx :: Ord k => [k] -> Tree k a -> Maybe (Tree k a)
lookupx [] tree             = Just tree
lookupx (k: ks) (Tree _ ch) = lookupx ks =<< M.lookup k ch


empty :: (Ord k, Monoid a) => Tree k a
empty = Tree mempty M.empty


singleton :: Monoid m => [k] -> m -> Tree k m
singleton [] x      = Tree x M.empty
singleton (p: ps) x = Tree mempty $ M.singleton p (singleton ps x)

insert :: (Ord k, Monoid a) => [k] -> a -> Tree k a -> Tree k a
insert path xx tree = singleton path xx <> tree

append :: (Ord k, Monoid a) => [k] -> Tree k a -> a -> Tree k a
append path tree xx = tree <> singleton path xx


fromList :: (Ord k, Monoid a) => [([k], a)] -> Tree k a
fromList = foldr (uncurry insert) empty

adjust :: Ord k => (a -> a) -> [k] -> Tree k a -> Tree k a
adjust fce [] (Tree x ch)       = Tree (fce x) ch
adjust fce (p : ps) (Tree x ch) = Tree x (M.adjust (adjust fce ps) p ch)

delete :: Ord k => [k] -> Tree k a -> Tree k a
delete [] tree              = tree
delete [p] (Tree x ch)      = Tree x $ M.delete p ch
delete (p : ps) (Tree x ch) = Tree x (M.adjust (delete ps) p ch)

delete' :: Ord k => [k] -> Tree k a -> Tree k a
delete' [] tree              = tree
delete' (p : ps) (Tree x ch) = Tree x (if null ps then M.delete p ch
                                                  else M.adjust (delete' ps) p ch)

adjustWithKey :: Ord k => ([k] -> a -> a) -> [k] -> Tree k a -> Tree k a
adjustWithKey f key = adj (f key) key
  where
    adj fce [] (Tree x ch)       = Tree (fce x) ch
    adj fce (p : ps) (Tree x ch) = Tree x (M.adjust (adj fce ps) p ch)


--update fce (p : ps) (Tree x ch) = Tree x (M.update (update fce ps) p ch)


{-
toDump :: Tree String Integer -> [String]
--toDump (LFile ree _) = [printRee ree]
toDump (Tree _ items) = ("    " ++) <$> (M.toList items >>= todump)
   where
      todump :: (String, Tree String Integer) -> [String]
      todump (filename, q@(LFile _ _)) = prependToFirst (filename ++ ": ") (toDump q)
      todump (filename, q@(LDir ree _)) =   ("/" ++ filename ++ " " ++ printRee ree) : toDump q

-}
