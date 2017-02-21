{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where



#include "qprelude/default.inc"

import           Lens.Micro ( (^.), at, (%~), mapped, _2)
import           Lens.Micro.Mtl ( (%=), (.=) )

import           Safe (headDef)

import qualified Reflex as R
import qualified Reflex.Host.App as RH

import qualified Brick.Widgets.Border.Style
import qualified Brick.Widgets.Border
import           Graphics.Vty(defAttr)
import           Brick.AttrMap(attrMap)
import           Graphics.Vty.Input.Events
import qualified Graphics.Vty as Vty
import           Brick.Widgets.Core
import           Brick.MainReflex
import           Brick.Types

import           UI.Butcher.Monadic        as Butcher
import           UI.Butcher.Monadic.Types  as Butcher
import           UI.Butcher.Monadic.Pretty as Butcher
import           UI.Butcher.Monadic.Param  as Butcher

import qualified Text.PrettyPrint as PP

import           Linewise
import           MainCommandParser



type Trigger a = a -> IO ()



main :: forall t . (t ~ R.SpiderTimeline R.Global) => IO ()
main = do
  -- persRead "./colint.pers"

  let finallyAct :: IO () = do
        return ()
        -- persWrite

  (`finally`finallyAct) $ R.runSpiderHost $ RH.hostApp $ do

    (shutdownE, shutdownT) <- RH.newExternalEvent

    linewise Seq.empty $ \commandE outDyn histB finE -> do

      -- tell ReflexHost to quit when linewise sends its shutdown-completion event.
      RH.performPostBuild_ $ do
        pure $ RH.infoQuit $ pure finE

      -- grab a event that fires once at start; currently used to do an initial
      -- (re)draw.
      postBuild <- RH.getPostBuild
      -- RH.performEvent_ $ postBuild <&> \() -> liftIO $ do
      --   postCliInit
      --   return ()

      -- Parse the line when the user presses enter. Here I use butcher to do
      -- the parsing, but you can do without that too and use only
      -- reflex-brick.
      let
        parseResE = flip (R.attachWith id) commandE $ histB <&> \hist inp ->
          either (Left . errorText inp)
                 (maybe (Left $ missingImplText inp) Right . _cmd_out)
            $ (\(_, _, x) -> x)
            $ mainCmdParserRun inp
            $ mainCmdParser (Text.unlines $ Foldable.toList hist)
                            (void . shutdownT)

      let (commandNoParseE, commandParseE) = R.fanEither parseResE

      -- execute the actions for successful input parses, retrieve an
      -- event for the asnychronous results
      ioResultE      <- RH.performEventAsync commandParseE

      -- the main program state/view is just a Seq of lines to which different
      -- events add stuff.
      outputLinesDyn <- R.foldDyn
        ( \addLines oldLines ->
          seqReverseTake 100 oldLines <> Seq.fromList addLines
        )
        (Seq.replicate 100 $ Text.singleton ' ')
        (fmap pure commandNoParseE <> ioResultE <> fmap mempty postBuild) -- empty update to force redraw at start

      -- the dynamic for current prompt line and interactive help based
      -- on the current it.
      let
        outParsedDyn
          = [ (specialPromptMay, cursorPos, promptStr, cdesc, pcRest)
            | (specialPromptMay, cursorPos, promptStr) <- outDyn
            , let
              (cdesc, pcRest, _)
                = mainCmdParserRun promptStr
                $ mainCmdParser Text.empty (void . shutdownT)
            ]

      -- to be used when users presses tab
      let completionB =
            R.current outParsedDyn <&> \(_, _, promptStr, cdesc, pcRest) ->
              completion (Text.unpack promptStr) cdesc pcRest

      -- and the complete cli output dynamic
      let
        windowDyn :: R.Dynamic (R.SpiderTimeline R.Global) (Widget Text) =
          [ withBorderStyle Brick.Widgets.Border.Style.unicode
            $ Brick.Widgets.Border.border
            $ padRight Brick.Types.Max
            $ vBox
                [ padTop Max $ textListWidget outputLines
                , hBox
                  [ case specialPromptMay of
                    Nothing -> str "> "
                    Just x  -> txt x
                  , showCursor (Text.pack "prompt") (Location (cursorPos, 0))
                    $ txt
                    $ if Text.null promptStr then Text.pack " " else promptStr
                  ]
                , helpWidget (Text.unpack promptStr) cdesc pcRest
                ]
          | (specialPromptMay, cursorPos, promptStr, cdesc, pcRest) <-
            outParsedDyn
          , outputLines <- outputLinesDyn
          ]

      return (shutdownE, completionB, windowDyn)
 where
  errorText :: Text -> ParsingError -> Text
  errorText inp err =
    (  Text.pack "input not understood '"
    <> inp
    <> Text.pack "' ("
    <> Text.pack (show $ _pe_messages err)
    <> Text.pack ", "
    <> Text.pack (show $ _pe_remaining err)
    <> Text.pack ")"
    )

  missingImplText :: Text -> Text
  missingImplText inp =
    Text.pack "no implementation for '" <> inp <> Text.pack "'"





--------------------
-- random helpers
--------------------

seqReverseTake :: Int -> Seq a -> Seq a
seqReverseTake i s = Seq.drop (Seq.length s - i) s

textListWidget :: Seq Text -> Widget n
textListWidget contentLines = Widget Greedy Greedy $ do
  ctx <- getContext
  let availLines = ctx ^. availHeightL
  render $ txt $ Text.unlines $ Foldable.toList $ seqReverseTake availLines
                                                                 contentLines


doc :: PP.Doc -> Widget n
doc d = Widget Greedy Fixed $ do
  ctx <- getContext
  let s = PP.renderStyle PP.style {PP.lineLength = availWidth ctx} d
  render $ str s

mainCmdParserRun
  :: Text
  -> CmdParser Identity a ()
  -> ( CommandDesc ()
     , String
     , Either
         ParsingError
         (CommandDesc a)
     )
mainCmdParserRun t p = case pcRest of
  InputString s -> (cdesc, s, eRes)
  InputArgs{}   -> error "unlikely (TM)"
 where
  (cdesc, pcRest, eRes) =
    Butcher.runCmdParserExt Nothing (Butcher.InputString $ Text.unpack t) p

helpWidget :: String -> CommandDesc a -> String -> Widget Text
helpWidget cmdline desc pcRest = vLimit 4 $ wid <=> fill ' '
 where
  wid = if
    | null cmdline -> doc helpStrShort
    | last cmdline == ' ' -> doc $ helpStrShort
    | otherwise           -> doc $ helpStr
  helpStr = if List.length optionLines > 4
    then PP.fcat $ List.intersperse (PP.text "|")  $ PP.text . fst <$> optionLines
    else PP.vcat $ optionLines <&> \case
      (s, "") -> PP.text s
      (s, h ) -> PP.text s PP.<> PP.text h
    where
      nameDesc = case _cmd_mParent desc of
        Nothing                        -> desc
        Just (_, parent) | null pcRest -> parent
        Just{}                         -> desc

      lastWord = reverse $ takeWhile (not . isSpace) $ reverse $ cmdline
      optionLines = -- a list of potential words that make sense, given
                      -- the current input.
        join
          [ [ r
            | Just r <- _cmd_children nameDesc <&> \(s, c) ->
              [ ( s
                , join $ join $
                  [ [ " ARGS" | not $ null $ _cmd_parts c ]
                  , [ " CMDS" | not $ null $ _cmd_children c ]
                  , [ ": " ++ show h | Just h <- [_cmd_help c] ]
                  ]
                )
              | lastWord `isPrefixOf` s
              ]
            ]
          , [ (s, e)
            | (s, e) <- transPartDesc =<< _cmd_parts nameDesc
            , lastWord `isPrefixOf` s
            ]
          ]
  helpStrShort = ppUsageWithHelp desc
  transPartDesc :: PartDesc -> [(String, [Char])]
  transPartDesc = \case
    PartLiteral s -> [(s, "")]
    PartVariable _ -> []
    -- TODO: we could handle seq of optional and such much better
    PartOptional x -> transPartDesc x
    PartAlts alts -> alts >>= transPartDesc
    PartSeq [] -> []
    PartSeq (x:_) -> transPartDesc x
    PartDefault _ x -> transPartDesc x
    PartSuggestion ss x -> [(s, "") | s <- ss] ++ transPartDesc x
    PartRedirect _ x -> transPartDesc x
    PartReorder xs -> xs >>= transPartDesc
    PartMany x -> transPartDesc x
    PartWithHelp _h x -> transPartDesc x -- TODO: handle help

completion :: String -> CommandDesc a -> String -> String
completion cmdline desc pcRest =
  List.drop (List.length lastWord) $ case choices of
    [] -> ""
    (c1:cr) ->
      headDef ""
        $ filter (\s -> List.all (s`isPrefixOf`) cr)
        $ reverse
        $ List.inits c1
 where
  nameDesc = case _cmd_mParent desc of
    Nothing                        -> desc
    Just (_, parent) | null pcRest -> parent
    Just{}                         -> desc
  lastWord = reverse $ takeWhile (not . isSpace) $ reverse $ cmdline
  choices =
    join
      [ [ r
        | Just r <-
          _cmd_children nameDesc <&> \(s, _) -> [ s | lastWord `isPrefixOf` s ]
        ]
      , [ s
        | s <- transPartDesc =<< _cmd_parts nameDesc
        , lastWord `isPrefixOf` s
        ]
      ]
  transPartDesc :: PartDesc -> [String]
  transPartDesc = \case
    PartLiteral  s      -> [s]
    PartVariable _      -> []
    -- TODO: we could handle seq of optional and such much better
    PartOptional x      -> transPartDesc x
    PartAlts     alts   -> alts >>= transPartDesc
    PartSeq      []     -> []
    PartSeq      (x:_)  -> transPartDesc x
    PartDefault    _  x -> transPartDesc x
    PartSuggestion ss x -> ss ++ transPartDesc x
    PartRedirect   _  x -> transPartDesc x
    PartReorder xs      -> xs >>= transPartDesc
    PartMany    x       -> transPartDesc x
    PartWithHelp _h x   -> transPartDesc x -- TODO: handle help     
