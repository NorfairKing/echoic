module Echoic.App.Render
  ( drawUI,
  )
where

import Brick
import Cursor.Brick.Text (selectedTextCursorWidget)
import qualified Data.Text as Text
import Echoic.App
import Echoic.Config

drawUI :: Config -> AppState -> [Widget ResourceName]
drawUI config s = case stateMode s of
  InputMode -> drawInputMode config s
  OutputMode -> drawOutputMode config s

drawInputMode :: Config -> AppState -> [Widget ResourceName]
drawInputMode config s =
  [ vBox $
      [ str "$ " <+> selectedTextCursorWidget MainViewport (stateInputBuffer s),
        str " "
      ]
        <> formatBindingsForUI bindings
  ]
  where
    bindings =
      globalBindingsList (configGlobalBindings config)
        <> inputBindingsList (configInputBindings config)

drawOutputMode :: Config -> AppState -> [Widget ResourceName]
drawOutputMode config s =
  [ vBox $
      case stateOutput s of
        Nothing ->
          [str "No output", str " "]
            <> formatBindingsForUI bindings
        Just ob ->
          let total = length (outputLines ob)
              idx = outputIndex ob
              currentLine =
                if null (outputLines ob)
                  then "(empty output)"
                  else Text.unpack (outputLines ob !! idx)
           in [ str $ "$ " <> Text.unpack (outputCommand ob),
                str $ "Line " <> show (idx + 1) <> "/" <> show (max 1 total),
                str " ",
                str currentLine,
                str " "
              ]
                <> formatBindingsForUI bindings
  ]
  where
    bindings =
      globalBindingsList (configGlobalBindings config)
        <> outputBindingsList (configOutputBindings config)

-- | Format a bindings list for UI display, one per line
formatBindingsForUI :: [(String, String)] -> [Widget ResourceName]
formatBindingsForUI = map (\(k, d) -> str $ k <> ": " <> d)
