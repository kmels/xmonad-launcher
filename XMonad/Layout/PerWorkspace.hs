{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PerWorkspace
-- Copyright   :  (c) Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts on a per-workspace basis.
-----------------------------------------------------------------------------

module XMonad.Layout.PerWorkspace (
                                    -- * Usage
                                    -- $usage

                                    onWorkspace
                                  ) where

import XMonad
import qualified XMonad.StackSet as W

import Control.Monad.State (gets)
import Data.Maybe (fromMaybe)

-- $usage
-- You can use this module by importing it into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.PerWorkspace
--
-- and modifying your layoutHook as follows:
--
-- > layoutHook = onWorkspace "foo" l1 $  -- layout l1 will be used on workspace "foo"
-- >              onWorkspace "bar" l2 $  -- layout l2 will be used on workspace "bar"
-- >              l3                      -- layout l3 will be used on all other workspaces.
--
-- Note that @l1@, @l2@, and @l3@ can be arbitrarily complicated layouts,
-- e.g. @(Full ||| smartBorders $ tabbed shrinkText defaultTConf ||| ...)@
--
-- In another scenario, suppose you wanted to have layouts A, B, and C
-- available on all workspaces, except that on workspace foo you want
-- layout D instead of C.  You could do that as follows:
--
-- > layoutHook = A ||| B ||| onWorkspace "foo" D C

-- | Specify one layout to use on a particular workspace, and another
--   to use on all others.  The second layout can be another call to
--   'onWorkspace', and so on.
onWorkspace :: (LayoutClass l1 a, LayoutClass l2 a)
               => WorkspaceId -- ^ the tag of the workspace to match
               -> (l1 a)      -- ^ layout to use on the matched workspace
               -> (l2 a)      -- ^ layout to use everywhere else
               -> PerWorkspace l1 l2 a
onWorkspace wsId l1 l2 = PerWorkspace wsId Nothing l1 l2

-- | Structure for representing a workspace-specific layout along with
--   a layout for all other workspaces.  We store the tag of the workspace
--   to be matched, and the two layouts.  Since layouts are stored\/tracked
--   per workspace, once we figure out which workspace we are on, we can
--   cache that information using a (Maybe Bool).  This is necessary
--   to be able to correctly implement the 'description' method of
--   LayoutClass, since a call to description is not able to query the
--   WM state to find out which workspace it was called in.
data PerWorkspace l1 l2 a = PerWorkspace WorkspaceId
                                        (Maybe Bool)
                                        (l1 a)
                                        (l2 a)
    deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (PerWorkspace l1 l2) a where

    -- do layout with l1, then return a modified PerWorkspace caching
    --   the fact that we're in the matched workspace.
    doLayout p@(PerWorkspace _ (Just True) lt _) r s = do
        (wrs, mlt') <- doLayout lt r s
        return (wrs, Just $ mkNewPerWorkspaceT p mlt')

    -- do layout with l1, then return a modified PerWorkspace caching
    --   the fact that we're not in the matched workspace.
    doLayout p@(PerWorkspace _ (Just False) _ lf) r s = do
        (wrs, mlf') <- doLayout lf r s
        return (wrs, Just $ mkNewPerWorkspaceF p mlf')

    -- figure out which layout to use based on the current workspace.
    doLayout (PerWorkspace wsId Nothing l1 l2) r s  = do
        t <- getCurrentTag
        doLayout (PerWorkspace wsId (Just $ wsId == t) l1 l2) r s

    -- handle messages; same drill as doLayout.
    handleMessage p@(PerWorkspace _ (Just True) lt _) m = do
        mlt' <- handleMessage lt m
        return . Just $ mkNewPerWorkspaceT p mlt'

    handleMessage p@(PerWorkspace _ (Just False) _ lf) m = do
        mlf' <- handleMessage lf m
        return . Just $ mkNewPerWorkspaceF p mlf'

    handleMessage (PerWorkspace wsId Nothing l1 l2) m = do
        t <- getCurrentTag
        handleMessage (PerWorkspace wsId (Just $ wsId == t) l1 l2) m

    description (PerWorkspace _ (Just True ) l1 _) = description l1
    description (PerWorkspace _ (Just False) _ l2) = description l2

    -- description's result is not in the X monad, so we have to wait
    -- until a doLayout or handleMessage for the information about
    -- which workspace we're in to get cached.
    description _ = "PerWorkspace"

-- | Construct new PerWorkspace values with possibly modified layouts.
mkNewPerWorkspaceT :: PerWorkspace l1 l2 a -> Maybe (l1 a) ->
                      PerWorkspace l1 l2 a
mkNewPerWorkspaceT (PerWorkspace wsId b lt lf) mlt' =
    (\lt' -> PerWorkspace wsId b lt' lf) $ fromMaybe lt mlt'

mkNewPerWorkspaceF :: PerWorkspace l1 l2 a -> Maybe (l2 a) ->
                      PerWorkspace l1 l2 a
mkNewPerWorkspaceF (PerWorkspace wsId b lt lf) mlf' =
    (\lf' -> PerWorkspace wsId b lt lf') $ fromMaybe lf mlf'

-- | Get the tag of the currently active workspace.
getCurrentTag :: X WorkspaceId
getCurrentTag = gets windowset >>= return . W.tag . W.workspace . W.current