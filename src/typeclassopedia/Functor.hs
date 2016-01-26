{- 1. Implement Functor instances for Either e and ((->) e). -}

instance Functor (Either e) where
  fmap _ (Left l) = Left l
  fmap g (Right r) = Right (g r)

instance Functor ((->) e) where
  fmap = (.)
