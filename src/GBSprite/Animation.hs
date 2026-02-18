-- | Animation frame sequencing and timing.
--
-- Animations are pure descriptions of how frames advance over time.
-- The actual frame selection is computed from a tick count, not
-- wall-clock time — the caller controls the tick rate.
module GBSprite.Animation
  ( -- * Types
    LoopMode (..),
    Animation (..),

    -- * Construction
    animation,
    loopAnimation,
    onceAnimation,
    pingPongAnimation,

    -- * Playback
    animationFrame,
    animationDone,
  )
where

-- | How an animation repeats.
data LoopMode
  = -- | Loop forever (restart from frame 0)
    Loop
  | -- | Play once and stop on last frame
    Once
  | -- | Bounce: 0,1,2,1,0,1,2,...
    PingPong
  deriving (Show, Eq)

-- | An animation description.
data Animation = Animation
  { -- | Ticks per frame (higher = slower)
    animFrameDelay :: !Int,
    -- | Total frame count
    animFrameCount :: !Int,
    -- | How to repeat
    animLoopMode :: !LoopMode
  }
  deriving (Show, Eq)

-- | Create an animation with explicit loop mode.
animation :: Int -> Int -> LoopMode -> Animation
animation = Animation

-- | Create a looping animation.
loopAnimation :: Int -> Int -> Animation
loopAnimation delay count = Animation delay count Loop

-- | Create a play-once animation.
onceAnimation :: Int -> Int -> Animation
onceAnimation delay count = Animation delay count Once

-- | Create a ping-pong animation.
pingPongAnimation :: Int -> Int -> Animation
pingPongAnimation delay count = Animation delay count PingPong

-- | Compute the current frame index for a given tick.
animationFrame :: Animation -> Int -> Int
animationFrame (Animation delay count mode) tick
  | count <= 0 = 0
  | delay <= 0 = 0
  | otherwise =
      let rawFrame = tick `div` delay
       in case mode of
            Loop -> rawFrame `mod` count
            Once -> min rawFrame (count - 1)
            PingPong ->
              let cycleLen = max 1 (count * 2 - 2)
                  pos = rawFrame `mod` cycleLen
               in if pos < count then pos else cycleLen - pos

-- | Check if a play-once animation has finished.
animationDone :: Animation -> Int -> Bool
animationDone (Animation delay count Once) tick =
  delay > 0 && count > 0 && tick `div` delay >= count - 1
animationDone _ _ = False
