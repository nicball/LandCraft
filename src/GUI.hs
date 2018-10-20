module GUI where

type Position = (Float, Float)

data Widget
    = Widget { widgetPosition :: Position
             , widgetWidth :: Float
             , widgetHeight :: Float
             , widgetRender :: Render ()
             , widgetOnMouse :: Position -> MouseAction -> IO ()
             , widgetOnKeyboard :: KeyboardAction -> IO ()
             }

data Window
    = Window { windowHandler :: GLFW.Window
             , windowWidgets :: [Widget]
             }
