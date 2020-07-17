{-# language CPP, ViewPatterns, OverloadedStrings, RecordWildCards #-}
module Pure.Gestures.Internal.Touch 
  ( Touch(..)
  , TouchId
  , Point
  , touches
  , touchIdentifier
  , touchScreenX, touchScreenY
  , touchClientX, touchClientY
  , touchPageX, touchPageY
  , touchRadiusX, touchRadiusY
  , touchRotationAngle
  , touchForce
  , touchClientPoint, touchScreenPoint, touchPagePoint
  ) where

import Pure hiding (touch)
import Data.IORef
import Data.Maybe
import System.IO.Unsafe

data Touch = Touch 
  { touchId :: TouchId
  , touch   :: JSV
  }

type TouchId = Int

#ifdef __GHCJS__  
foreign import javascript unsafe
  "$r = $1.item($2)" 
    touch_item_js 
      :: JSV -> Int -> IO JSV
#endif

readTouches :: JSV -> [Touch]
readTouches ts = unsafePerformIO $ do
#ifdef __GHCJS__
  case ts .# "length" of
    Just l  -> 
      for [0..l - 1] $ \i -> do
        touch <- touch_item_js ts i
        let touchId = touchIdentifier touch
        pure Touch {..}
    Nothing -> 
      pure []
#else
  pure []
#endif

touches :: Evt -> [Touch]
touches ev@(evtObj -> obj) = 
  fromMaybe [] $ readTouches <$> 
    (obj .# "touches")

touchIdentifier :: JSV -> TouchId
touchIdentifier t 
  | Just i <- t .# "identifier" = i
  | otherwise = error "Touch['identifier']: not found"

touchScreenX :: Touch -> Double
touchScreenX t
  | Just i <- touch t .# "screenX" = i
  | otherwise = error "Touch['screenX']: not found"

touchScreenY :: Touch -> Double
touchScreenY t
  | Just i <- touch t .# "screenY" = i
  | otherwise = error "Touch['screenY']: not found"

touchClientX :: Touch -> Double
touchClientX t
  | Just i <- touch t .# "clientX" = i
  | otherwise = error "Touch['clientX']: not found"

touchClientY :: Touch -> Double
touchClientY t
  | Just i <- touch t .# "clientY" = i
  | otherwise = error "Touch['clientY']: not found"

touchPageX :: Touch -> Double
touchPageX t
  | Just i <- touch t .# "pageX" = i
  | otherwise = error "Touch['pageX']: not found"

touchPageY :: Touch -> Double
touchPageY t
  | Just i <- touch t .# "pageY" = i
  | otherwise = error "Touch['pageY']: not found"

touchRadiusX :: Touch -> Double
touchRadiusX t
  | Just i <- touch t .# "radiusX" = i
  | otherwise = error "Touch['radiusX']: not found"

touchRadiusY :: Touch -> Double
touchRadiusY t
  | Just i <- touch t .# "radiusY" = i
  | otherwise = error "Touch['radiusY']: not found"

touchRotationAngle :: Touch -> Double
touchRotationAngle t
  | Just i <- touch t .# "rotationAngle" = i
  | otherwise = error "Touch['rotationAngle']: not found"

touchForce :: Touch -> Double
touchForce t
  | Just i <- touch t .# "force" = i
  | otherwise = error "Touch['force']: not found"

type Point = (Double,Double)

touchClientPoint :: Touch -> Point
touchClientPoint = (,) <$> touchClientX <*> touchClientY

touchScreenPoint :: Touch -> Point
touchScreenPoint = (,) <$> touchScreenX <*> touchScreenY

touchPagePoint :: Touch -> Point
touchPagePoint = (,) <$> touchPageX <*> touchPageY