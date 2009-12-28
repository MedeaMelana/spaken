module GTK where

import Shape
import MonadSpaken

import Data.List (partition)
import qualified Data.Set as Set

import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Abstract.Widget
import Graphics.Rendering.Cairo
-- import Graphics.Rendering.Cairo.Matrix

showMe :: ToShapeHist (ShapeHist i) -> IO ()
showMe thing = do
  initGUI
  window <- windowNew
  set window
    [ containerBorderWidth := 0
    , windowTitle := "Spaken"
    -- , windowWindowPosition := WinPosCenter
    ]
  onDestroy window mainQuit

  canv <- drawingAreaNew
  canv `onSizeRequest` return (Requisition 300 300) -- minimum ofzo
  
  canv `onExpose` updateCanvas canv thing
  
  contain <- hBoxNew False 5 -- geen idee
  
  window `containerAdd` contain

  -- layout
  boxPackStartDefaults contain canv
  
  -- window `containerAdd` hBox
  -- boxPackStart hBox scaleCarAmount PackNatural 0
  -- boxPackStart hBox vBox PackGrow 0
  -- boxPackStart vBox desenho PackGrow 0
  -- boxPackStart vBox hSeparator PackNatural 0
  -- boxPackStart vBox hButtonBox PackNatural 0
  -- buttonBoxSetLayout hButtonBox ButtonboxSpread
  -- mapM (boxPackStartDefaults hButtonBox) [buttonReset,buttonAbout,buttonQuit]

  widgetShowAll window
  mainGUI

updateCanvas :: DrawingArea -> ToShapeHist (ShapeHist i) -> Event -> IO Bool
updateCanvas canv thing _ = do
  draw <- widgetGetDrawWindow canv
  renderWithDrawable draw $ do
    translate 10 10
    let sh = toShapeHist thing
    let (points, stuff) = partition isPoint $ Set.toList $ history sh
    mapM_ (drawShape colorHist) (stuff ++ points)
    drawShape colorOutput $ someShape $ shape sh
  return True

colorOutput = setSourceRGB 0 0.2 0.8
colorHist   = setSourceRGB 0.8 0.2 0.2

example :: MonadSpaken m r => m (r Line)
example = do
  p1 <- point (50,50)
  p2 <- point (80,40)
  c1 <- circle p1 p2
  c2 <- circle p2 p1
  ps <- intersectCC c1 c2
  (l1, l2) <- bothPoints ps
  l <- line l1 l2
  return l

drawShape :: Render () -> SomeShape -> Render ()
drawShape neutral shape = case shape of
  (APoint t (x,y)) -> do colorFor neutral t ; drawCircle x y 3
  (ALine (x1,y1) (x2,y2)) ->
    do neutral
       moveTo x1 y1
       lineTo x2 y2
       stroke
  (ACircle (x,y) r) -> do neutral ; drawCircle x y r
  (ATwoPoints p1 p2) -> do drawShape neutral p1 ; drawShape neutral p2

colorFor n Postulated = setSourceRGB 0.2 0.8 0.2
colorFor n Derived    = n -- setSourceRGB 0.2 0.4 1

isPoint (APoint _ _) = True
isPoint _            = False

-- gejat van Drawing2:

drawCircle x y r = do
  arc x y r 0 (2 * pi)
  fillStroke

fillStroke = stroke -- do fillPreserve ; stroke

