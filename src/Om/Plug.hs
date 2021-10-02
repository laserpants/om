module Om.Plug
  ( Plugin
  , plugin
  , pluginVarHook
  , pluginConHook
  , pluginPatHook
  ) where

import Om.Eval
import Om.Util

newtype VarHookPlug p m = VarHookPlug (VarHook p m)
newtype ConHookPlug p m = ConHookPlug (ConHook p m)
newtype PatHookPlug p m = PatHookPlug (PatHook p m)

instance (Monad m) => Semigroup (VarHookPlug p m) where
    VarHookPlug p <> VarHookPlug q = VarHookPlug r
      where
        r name = p name >>= maybe (q name) (pure . Just)

instance (Monad m) => Monoid (VarHookPlug p m) where
    mempty = VarHookPlug (\_ -> pure Nothing)

instance (Monad m) => Semigroup (ConHookPlug p m) where
    ConHookPlug p <> ConHookPlug q = ConHookPlug r
      where
        r name = p name >>= maybe (q name) (pure . Just)

instance (Monad m) => Monoid (ConHookPlug p m) where
    mempty = ConHookPlug (\_ -> pure Nothing)

instance (Monad m) => Semigroup (PatHookPlug p m) where
    PatHookPlug p <> PatHookPlug q = PatHookPlug r
      where
        r pats val = p pats val >>= maybe (q pats val) (pure . Just)

instance (Monad m) => Monoid (PatHookPlug p m) where
    mempty = PatHookPlug (\_ _ -> pure Nothing)

data Plugin p m = Plugin (VarHookPlug p m) (ConHookPlug p m) (PatHookPlug p m)

pluginVarHook :: Plugin p m -> VarHook p m
pluginVarHook (Plugin (VarHookPlug hook) _ _) = hook

pluginConHook :: Plugin p m -> ConHook p m
pluginConHook (Plugin _ (ConHookPlug hook) _) = hook

pluginPatHook :: Plugin p m -> PatHook p m
pluginPatHook (Plugin _ _ (PatHookPlug hook)) = hook

instance (Monad m) => Semigroup (Plugin p m) where
    Plugin p1 p2 p3 <> Plugin q1 q2 q3 = Plugin (p1 <> q1) (p2 <> q2) (p3 <> q3)

instance (Monad m) => Monoid (Plugin p m) where
    mempty = Plugin mempty mempty mempty

plugin :: (Monad m) => Maybe (VarHook p m) -> Maybe (ConHook p m) -> Maybe (PatHook p m) -> Plugin p m
plugin vh ch ph = Plugin (maybe mempty VarHookPlug vh) (maybe mempty ConHookPlug ch) (maybe mempty PatHookPlug ph)
