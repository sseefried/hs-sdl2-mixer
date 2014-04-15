#include "SDL_mixer.h"
module Sound.SDL.Mixer.Types
  ( Chunk(..)
  , MusicStruct
  , Music
  , MusicType(..)
  , mkFinalizedMusic
  , Channel
  , Volume
  , makeVolume
  , unwrapVolume
  , Fading(..)
  ) where

import Foreign
import Control.Applicative

data MusicStruct
type Music = ForeignPtr MusicStruct

foreign import ccall unsafe "&Mix_FreeMusic"
  mixFreeMusic' :: FunPtr (Ptr MusicStruct -> IO ())

mkFinalizedMusic :: Ptr MusicStruct -> IO Music
mkFinalizedMusic = newForeignPtr mixFreeMusic'

data Chunk
   = Chunk { chunkAllocated :: #{type int}
           , chunkABuf :: Ptr #{type Uint8}
           , chunkAlen :: #{type Uint32}
           , chunkVolume :: #{type Uint8}
           }
    deriving (Eq, Show)

instance Storable Chunk where
  sizeOf = const #{size Mix_Chunk}
  alignment = const 4
  poke ptr Chunk{..} = do
    #{poke Mix_Chunk, allocated} ptr chunkAllocated
    #{poke Mix_Chunk, abuf} ptr chunkABuf
    #{poke Mix_Chunk, alen} ptr chunkAlen
    #{poke Mix_Chunk, volume} ptr chunkVolume
  peek ptr = Chunk
    <$> #{peek Mix_Chunk, allocated} ptr
    <*> #{peek Mix_Chunk, abuf} ptr
    <*> #{peek Mix_Chunk, alen} ptr
    <*> #{peek Mix_Chunk, volume} ptr

data MusicType
   = NONE | CMD | WAV | MOD | MID | OGG | MP3 | MP3_MAD | FLAC | MODPLUG
   deriving (Show, Eq)

type Channel = Int

newtype Volume
      = Volume { unwrapVolume :: Int }
      deriving (Eq, Show)

maxVolume :: Int
maxVolume = #{const MIX_MAX_VOLUME}

makeVolume :: Int -> Volume
makeVolume i
  | i < -1        = error "volume is too low"
  | i > maxVolume = error "volume is too high"
  | otherwise     = Volume i

data Fading
   = NoFading | FadingOut | FadingIn
   deriving (Show, Eq)

