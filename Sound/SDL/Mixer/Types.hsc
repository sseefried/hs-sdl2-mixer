#include "SDL_mixer.h"
module Sound.SDL.Mixer.Types
  ( Chunk(..)
  , MusicStruct
  , Music
  , mkFinalizedMusic
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

