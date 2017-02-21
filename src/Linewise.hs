{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Linewise
  ( linewise
  )
where



#include "qprelude/default.inc"

import qualified Brick.MainReflex as Brick
import qualified Reflex as R
import qualified Reflex.Host.Class as R
import qualified Reflex.PerformEvent.Base as R
import qualified Reflex.Host.App as RH
import qualified Graphics.Vty as V

import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)

-- import qualified Control.Monad.Ref as Ref

-- import           LavaLantern.ProcCore.PersistentState

import           Brick.Types                      ( Widget )



data LinewiseState = LinewiseState
  { _lw_hist     :: Seq Text
  , _lw_histLen  :: Int
  , _lw_histRef  :: Maybe Int
  , _lw_line     :: Text
  , _lw_cursor   :: Int -- column of the cursor
  , _lw_rsearch  :: Maybe (Text, [Text]) -- query, stream of candidates
  }

linewise
  :: forall n t
   . (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => Seq Text
  -> (  R.Event t Text -- command string executed by user
     -> R.Dynamic t (Maybe Text, Int, Text) -- custom prompt, cursor location, current commandstring
     -> R.Behavior t (Seq Text) -- history
     -> R.Event t ()   -- post-shutdown
     -> RH.AppHost t
         ( R.Event t () -- shutdown trigger
         , R.Behavior t String -- tab-completion value
         , R.Dynamic t (Widget n)
         )
     )
  -> RH.AppHost t ()
linewise initialHist callB = Brick.brickWrapper $ \vtyEvent finE _ -> mdo

  -- linewisePMVar <- liftIO $ globalPersMVar >>= persNest "colint" >>= persNest "linewise"
  -- loadedHist <- liftIO $ persGetDef "history" Seq.empty linewisePMVar

  let initState = LinewiseState
        { _lw_hist    = initialHist
        , _lw_histLen = Seq.length initialHist
        , _lw_histRef = Nothing
        , _lw_line    = Text.empty
        , _lw_cursor  = 0
        , _lw_rsearch = Nothing
        }

  stateDyn <-
    R.foldDynM id initState
      $ R.fforMaybe vtyEvent
      $ fmap
      $ \event state -> do
        completion <- R.sample completionB
        return $ case _lw_rsearch state of
          Nothing -> newStateNoSearch completion state event
          Just (query, candidates) ->
            newStateSearch state event query candidates

  let
    outDyn = stateDyn <&> \state ->
      (_lw_rsearch state
        <&> \(q, _) -> Text.pack "(reverse-i-search: " <> q <> Text.pack ") ", _lw_cursor
        state, _lw_line state)

  let
    outEvent =
      flip (R.attachWithMaybe id) vtyEvent
        $   (R.current stateDyn)
        <&> \state -> \case
              Just (V.EvKey V.KEnter []) | not (Text.null $ _lw_line state) ->
                Just $ _lw_line state
              Just (V.EvKey V.KEsc []) | not (isJust $ _lw_rsearch state) ->
                Just $ Text.pack "exit"
              _ -> Nothing

  -- TODO: support passing the history back to the user
  -- finE2 <- RH.performEvent $ flip R.tag finE $ R.current stateDyn <&> \s -> do
  --   liftIO $ persSet "history" (_lw_hist s) linewisePMVar

  (shutdownTrigger, completionB, outWidget) <- callB outEvent outDyn (_lw_hist <$> R.current stateDyn) finE

  refreshHelperDyn             <- R.holdDyn undefined vtyEvent

  return
    (shutdownTrigger, pure <$> outWidget <* refreshHelperDyn, pure $ listToMaybe, pure
      $ attrMap defAttr [])
 where

  genHistory :: Text -> Seq Text -> [Text]
  genHistory query
    | Text.null query
    = const []
    | otherwise
    = List.filter (query`Text.isInfixOf`) . Foldable.toList . Seq.reverse

  newStateNoSearch :: String -> LinewiseState -> V.Event -> LinewiseState
  newStateNoSearch completion state event = case event of
    V.EvKey V.KEnter [] -> state
      { _lw_hist    = _lw_hist state Seq.|> _lw_line state
      , _lw_histLen = _lw_histLen state + 1
      , _lw_histRef = Nothing
      , _lw_line    = Text.empty
      , _lw_cursor  = 0
      }
    V.EvKey (V.KChar '\t') _ -> state
      { _lw_line = _lw_line state <> Text.pack completion
      , _lw_cursor = _lw_cursor state + List.length completion
      }
    V.EvKey (V.KChar c) [] -> state
      { _lw_line = let (a, b) = Text.splitAt (_lw_cursor state) (_lw_line state)
                   in  a <> Text.singleton c <> b
      , _lw_cursor = _lw_cursor state + 1
      }
    V.EvKey V.KBS [] -> if _lw_cursor state == 0
      then state
      else state
        { _lw_line   = let
                         (a, b) = Text.splitAt (_lw_cursor state) (_lw_line state)
                       in
                         Text.take (Text.length a - 1) a <> b
        , _lw_cursor = _lw_cursor state - 1
        }
    V.EvKey V.KBS [V.MAlt] ->
      let line' = Text.dropWhileEnd (not . isSpace)
                $ Text.dropWhileEnd (isSpace)
                $ _lw_line state
      in state
      { _lw_line   = line'
      , _lw_cursor = Text.length line'
      }
    V.EvKey V.KBS [V.MMeta] ->
      let line' = Text.dropWhileEnd (not . isSpace)
                $ Text.dropWhileEnd (isSpace)
                $ _lw_line state
      in state
      { _lw_line   = line'
      , _lw_cursor = Text.length line'
      }
    V.EvKey V.KDel [] -> state
      { _lw_line = let (a, b) = Text.splitAt (_lw_cursor state) (_lw_line state)
                   in  a <> Text.drop 1 b
      }
    V.EvKey V.KUp [] -> case (_lw_histLen state, _lw_histRef state) of
      (0, _) -> state
      (l, Nothing) ->
        let line = _lw_hist state `Seq.index` (l - 1)
        in  state { _lw_histRef = Just (l - 1)
                  , _lw_line    = line
                  , _lw_cursor  = Text.length line
                  }
      (_, Just cur) -> if cur == 0
        then state
        else
          let line = _lw_hist state `Seq.index` (cur - 1)
          in  state { _lw_histRef = Just (cur - 1)
                    , _lw_line    = line
                    , _lw_cursor  = Text.length line
                    }
    V.EvKey V.KDown [] -> case (_lw_histLen state, _lw_histRef state) of
      (0, _       ) -> state
      (_, Nothing ) -> state
      (l, Just cur) -> if cur + 1 == l
        then state { _lw_histRef = Nothing
                   , _lw_line    = Text.empty
                   , _lw_cursor  = 0
                   }
        else
          let line = _lw_hist state `Seq.index` (cur + 1)
          in  state { _lw_histRef = Just (cur + 1)
                    , _lw_line    = line
                    , _lw_cursor  = Text.length line
                    }
    V.EvKey V.KLeft [] -> state { _lw_cursor = max 0 $ _lw_cursor state - 1 }
    V.EvKey V.KRight [] ->
      state
        { _lw_cursor = min (Text.length $ _lw_line state) (_lw_cursor state + 1)
        }
    V.EvKey V.KHome [] -> state { _lw_cursor = 0 }
    V.EvKey V.KEnd [] -> state { _lw_cursor = Text.length (_lw_line state) }
    V.EvKey (V.KChar 'r') [V.MCtrl] -> state
      { _lw_line    = Text.empty
      , _lw_rsearch = Just (Text.empty, [])
      , _lw_cursor  = 0
      }
    V.EvKey (V.KChar 'k') [V.MCtrl] ->
      state { _lw_line = Text.take (_lw_cursor state) (_lw_line state) }
    V.EvKey (V.KChar 'u') [V.MCtrl] -> state
      { _lw_line   = Text.drop (_lw_cursor state) (_lw_line state)
      , _lw_cursor = 0
      }
    _                               -> state

  newStateSearch :: LinewiseState -> V.Event -> Text -> [Text] -> LinewiseState
  newStateSearch state event query candidates = case event of
    V.EvKey V.KEnter [] -> state
      { _lw_hist    = _lw_hist state
        Seq.|> ( case candidates of
                 (cand:_) -> cand
                 []       -> Text.empty
               )
      , _lw_histLen = _lw_histLen state + 1
      , _lw_histRef = Nothing
      , _lw_line    = Text.empty
      , _lw_cursor  = 0
      , _lw_rsearch = Nothing
      }
    V.EvKey (V.KChar '\t') [] -> state
    V.EvKey (V.KChar c) [] ->
      let query'   = Text.snoc query c
          newCands = genHistory query' $ _lw_hist state
      in  state
            { _lw_rsearch = Just (query', newCands)
            , _lw_line    = case newCands of
              []       -> Text.empty
              (cand:_) -> cand
            }
    V.EvKey V.KBS   [] -> if Text.length query == 0
      then state
      else
        let query'   = Text.init query
            newCands = genHistory query' $ _lw_hist state
        in  state
              { _lw_rsearch = Just (query', newCands)
              , _lw_line    = case newCands of
                []       -> Text.empty
                (cand:_) -> cand
              }
    V.EvKey V.KDel  [] -> state
    V.EvKey V.KUp   [] -> case (_lw_histLen state, _lw_histRef state) of
      (0, _) -> state
      (l, Nothing) ->
        let line = _lw_hist state `Seq.index` (l - 1)
        in  state { _lw_histRef = Just (l - 1)
                  , _lw_line    = line
                  , _lw_cursor  = Text.length line
                  , _lw_rsearch = Nothing
                  }
      (_, Just cur) -> if cur == 0
        then state
        else
          let line = _lw_hist state `Seq.index` (cur - 1)
          in  state { _lw_histRef = Just (cur - 1)
                    , _lw_line    = line
                    , _lw_cursor  = Text.length line
                    , _lw_rsearch = Nothing
                    }
    V.EvKey V.KDown [] -> case (_lw_histLen state, _lw_histRef state) of
      (0, _       ) -> state { _lw_rsearch = Nothing }
      (_, Nothing ) -> state { _lw_rsearch = Nothing }
      (l, Just cur) -> if cur + 1 == l
        then state { _lw_histRef = Nothing
                   , _lw_line    = Text.empty
                   , _lw_cursor  = 0
                   , _lw_rsearch = Nothing
                   }
        else
          let line = _lw_hist state `Seq.index` (cur + 1)
          in  state { _lw_histRef = Just (cur + 1)
                    , _lw_line    = line
                    , _lw_cursor  = Text.length line
                    , _lw_rsearch = Nothing
                    }
    V.EvKey V.KLeft [] -> state
      { _lw_cursor  = 0
      , _lw_rsearch = Nothing
      , _lw_line    = case candidates of
        (cand:_) -> cand
        _        -> Text.empty
      }
    V.EvKey V.KRight [] ->
      let line = case candidates of
            (cand:_) -> cand
            _        -> Text.empty
      in  state { _lw_cursor  = Text.length line
                , _lw_rsearch = Nothing
                , _lw_line    = line
                }
    V.EvKey V.KHome [] -> state
      { _lw_cursor  = 0
      , _lw_rsearch = Nothing
      , _lw_line    = case candidates of
        (cand:_) -> cand
        _        -> Text.empty
      }
    V.EvKey V.KEnd [] ->
      let line = case candidates of
            (cand:_) -> cand
            _        -> Text.empty
      in  state { _lw_cursor  = Text.length line
                , _lw_rsearch = Nothing
                , _lw_line    = line
                }
    V.EvKey (V.KChar 'r') [V.MCtrl] -> state
      -- TODO: more candidates
    V.EvKey V.KEsc [] ->
      state { _lw_cursor = 0, _lw_line = Text.empty, _lw_rsearch = Nothing }
    _ -> state
