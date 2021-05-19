{-# language CPP, PatternSynonyms, RecordWildCards, OverloadedStrings, DeriveGeneric, DeriveAnyClass, BangPatterns, ScopedTypeVariables, ViewPatterns #-}
module Pure.Gestures.Zoom 
  ( ZoomEvent(..) 
  , Centroid
  , Point
  , onZoom
  ) where

import Pure.Gestures.Internal.Storage as Store
import Pure.Gestures.Internal.Touch

import Pure hiding (passive)
import Pure.Data.JSON
import Pure.Data.Lifted (preventDefault,Options(..))

import Data.Maybe
import Data.List (foldl')
import GHC.Generics

type Centroid = Point

data ZoomEvent = ZoomEvent
  { origin    :: Centroid
  , direction :: Double
  } deriving (Generic,ToJSON,FromJSON)

data ZoomData = ZoomData
  { clientOrigin :: Point
  , lastA :: (TouchId,Point)
  , lastB :: (TouchId,Point)
  } deriving (Generic,ToJSON,FromJSON)

zoomData :: Touch -> Touch -> ZoomData
zoomData t1 t2 =
  ZoomData
    (centroid [touchClientPoint t1,touchClientPoint t2])
    (touchId t1,touchClientPoint t1)
    (touchId t2,touchClientPoint t2)

onZoom :: HasFeatures a => (ZoomEvent -> IO ()) -> a -> a
onZoom f = screenSupport . mouseSupport . gestureSupport
  where
    screenSupport = 
        OnTouchStart zoomTouchStart 
      . OnTouchMove (zoomTouchMove f) 
      . OnTouchEnd zoomTouchEnd

    mouseSupport =
        OnWheel (zoomWheel f)
      . OnMouseMove zoomMouseMove

    gestureSupport =
        OnGestureStart zoomGestureStart
      . OnGestureChange (zoomGestureChange f)
      . OnGestureEnd zoomGestureEnd

zoomGestureStart :: Evt -> IO ()
zoomGestureStart (evtObj -> o) = do
  case o .# "scale" of
    Just (i :: Double) -> do
      store "pure-gestures-zoom-gesture" i
    _ ->
      pure ()

zoomGestureChange :: (ZoomEvent -> IO ()) -> Evt -> IO ()
zoomGestureChange f (evtObj -> o) = do
  preventDefault o
  case o .# "scale" of
    Just (i :: Double)  -> do
      morigin <- retrieve "pure-gestures-zoom-origin"
      mgesture <- retrieve "pure-gestures-zoom-gesture"
      case (morigin,mgesture) of
        (Just origin,Just g) -> do
          store "pure-gestures-zoom-gesture" i
          let direction = negate (signum (g - i))
          f ZoomEvent {..}
        _ ->
          pure ()
    _ -> 
      pure ()

zoomGestureEnd :: Evt -> IO ()
zoomGestureEnd (evtObj -> o) = do
  preventDefault o
  remove "pure-gestures-zoom-gesture"

zoomTouchStart :: Evt -> IO ()
zoomTouchStart ev
  | [a,b] <- touches ev
  = store "pure-gestures-zoom-touch" (zoomData a b)
  | otherwise 
  = remove "pure-gestures-zoom-touch"

zoomTouchMove :: (ZoomEvent -> IO ()) -> Evt -> IO ()
zoomTouchMove f ev
  | [t1,t2] <- touches ev
  = do mpd <- retrieve "pure-gestures-zoom-touch"
       case mpd of
         Just pd | Just (pd',p) <- createZoomEvent pd (t1,t2) 
           -> do store "pure-gestures-zoom-touch" pd'
                 f p
         _ -> pure ()
  | otherwise 
  = remove "pure-gestures-zoom-touch"

zoomTouchEnd :: Evt -> IO ()
zoomTouchEnd _ = remove "pure-gestures-zoom-touch"

zoomMouseMove :: Evt -> IO ()
zoomMouseMove (evtObj -> o) =
  case (o .# "clientX",o .# "clientY") of
    (Just (x :: Double),Just (y :: Double)) -> do
      store "pure-gestures-zoom-origin" (x,y)
    _ -> 
      pure ()

zoomWheel :: (ZoomEvent -> IO ()) -> Evt -> IO ()
zoomWheel f (evtObj -> e) = do
  preventDefault e
  morigin <- retrieve "pure-gestures-zoom-origin"
  case (morigin,e .# "wheelDeltaY") of
    (Just origin,Just d) ->
      let direction = signum d
      in f ZoomEvent {..}
    _ -> pure ()

createZoomEvent :: ZoomData -> (Touch,Touch) -> Maybe (ZoomData,ZoomEvent)
createZoomEvent pd@(ZoomData origin (t1,o1) (t2,o2)) (n1,n2)
  | t1 == touchId n1
  , t2 == touchId n2
  , p1 <- touchClientPoint n1
  , p2 <- touchClientPoint n2
  , clientOrigin <- origin
  , d1 <- dist origin p1 - dist origin o1 
  , d2 <- dist origin p2 - dist origin o2
  , lastA <- (t1,p1)
  , lastB <- (t2,p2)
  , direction <- signum ((d1 + d2) / 2)
  = Just (ZoomData {..},ZoomEvent {..})

  | t1 == touchId n1
  , t2 == touchId n2
  = createZoomEvent pd (n2,n1)

  | otherwise = Nothing

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) =
  let f a b = (b - a) ^ 2
  in sqrt ( f x1 x2 + f y1 y2 )

centroid :: [Point] -> Point
centroid = process . foldl' go (0,0,0)
  where
    process (n,x,y) = (x / n,y / n)
    go (!n,!xs,!ys) (x,y) = (n + 1,xs + x,ys + y)