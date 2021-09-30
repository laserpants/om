module Om.Plug
  ( Plugin
  , plugin
  , lookupHooks
  , patternHooks
  ) where

import Om.Eval
import Om.Util

newtype LookupHookPlug p m  = LookupHookPlug (LookupHook p m)
newtype PatternHookPlug p m = PatternHookPlug (PatternHook p m)

instance (Monad m) => Semigroup (LookupHookPlug p m) where
    LookupHookPlug p <> LookupHookPlug q = LookupHookPlug r
      where
        r name = p name >>= maybe (q name) (pure . Just)

instance (Monad m) => Monoid (LookupHookPlug p m) where
    mempty = LookupHookPlug (\_ -> pure Nothing)

instance (Monad m) => Semigroup (PatternHookPlug p m) where
    PatternHookPlug p <> PatternHookPlug q = PatternHookPlug r
      where
        r pats val = p pats val >>= maybe (q pats val) (pure . Just)

instance (Monad m) => Monoid (PatternHookPlug p m) where
    mempty = PatternHookPlug (\_ _ -> pure Nothing)

data Plugin p m = Plugin (LookupHookPlug p m) (PatternHookPlug p m)

lookupHooks :: Plugin p m -> LookupHook p m
lookupHooks (Plugin (LookupHookPlug p) _) = p

patternHooks :: Plugin p m -> PatternHook p m
patternHooks (Plugin _ (PatternHookPlug p)) = p

instance (Monad m) => Semigroup (Plugin p m) where
    Plugin p1 p2 <> Plugin q1 q2 = Plugin (p1 <> q1) (p2 <> q2)

instance (Monad m) => Monoid (Plugin p m) where
    mempty = Plugin mempty mempty

plugin :: (Monad m) => Maybe (LookupHook p m) -> Maybe (PatternHook p m) -> Plugin p m
plugin a b = Plugin (maybe mempty LookupHookPlug a) (maybe mempty PatternHookPlug b)
