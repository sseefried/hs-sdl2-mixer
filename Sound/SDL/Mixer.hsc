#include "SDL_mixer.h"
module Sound.SDL.Mixer
  ( version
  , initFLAC
  , initMOD
  , initMODPLUG
  , initMP3
  , initOGG
  , initFLUIDSYNTH
  , musicTypeNONE
  , musicTypeCMD
  , musicTypeWAV
  , musicTypeMOD
  , musicTypeMID
  , musicTypeOGG
  , musicTypeMP3
  , musicTypeMAD
  , musicTypeFLAC
  , musicTypeMODPLUG
  , init
  , quit
  , openAudio
  , allocateChannels
  , querySpec
  , loadWAVRW
  , loadMUS
  , loadMUSRW
  , loadMUSTypeRW
  , quickLoadWav
  , quickLoadRAW
  ) where

import Foreign
import Foreign.C.String
import Prelude hiding (init)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Audio (AudioFormat(..), fromAudioFormat, toAudioFormat)
import Graphics.UI.SDL.Error (getError)
import Sound.SDL.Mixer.Types

-- Error handling until something better is decided on in the main lib.
handleErrorI :: (Num a, Ord a) => String -> a -> (a -> IO b) -> IO b
handleErrorI fname i fn
  | i < 0     = fn i
  | otherwise = (\err -> error $ fname ++ ": " ++ show err) =<< getError
{-# INLINE handleErrorI #-}

-- | (Major, Minor, Patchlevel)
version :: (Int, Int, Int)
version = ( #{const SDL_MIXER_MAJOR_VERSION}
          , #{const SDL_MIXER_MINOR_VERSION}
          , #{const SDL_MIXER_PATCHLEVEL}
          )

newtype MixInitFlag
      = MixInitFlag { unwrapMixInitFlag :: #{type int} }

#{enum MixInitFlag, MixInitFlag
 , initFLAC = MIX_INIT_FLAC
 , initMOD = MIX_INIT_MOD
 , initMODPLUG = MIX_INIT_MODPLUG
 , initMP3 = MIX_INIT_MP3
 , initOGG = MIX_INIT_OGG
 , initFLUIDSYNTH = MIX_INIT_FLUIDSYNTH
 }

combineMixInitFlag :: [MixInitFlag] -> MixInitFlag
combineMixInitFlag = MixInitFlag . foldr ((.|.) . unwrapMixInitFlag) 0

newtype MusicType
      = MusicType { unwrapMusicType :: #{type Mix_MusicType} }

#{enum MusicType, MusicType
 , musicTypeNONE = MUS_NONE
 , musicTypeCMD = MUS_CMD
 , musicTypeWAV = MUS_WAV
 , musicTypeMOD = MUS_MOD
 , musicTypeMID = MUS_MID
 , musicTypeOGG = MUS_OGG
 , musicTypeMP3 = MUS_MP3
 , musicTypeMAD = MUS_MP3_MAD
 , musicTypeFLAC = MUS_FLAC
 , musicTypeMODPLUG = MUS_MODPLUG
 }

foreign import ccall unsafe "Mix_Init"
  mixInit' :: #{type int} -> IO ()

init :: [MixInitFlag] -> IO ()
init flags =
  let flags' = unwrapMixInitFlag $ combineMixInitFlag flags
  in mixInit' flags'

foreign import ccall unsafe "Mix_Quit"
  quit :: IO ()

foreign import ccall unsafe "Mix_OpenAudio"
  mixOpenAudio' :: #{type int} -> #{type Uint16} -> #{type int} -> #{type int} -> IO #{type int}

openAudio :: Int -> AudioFormat -> Int -> Int -> IO ()
openAudio freq format channels chunksize = do
  let freq'      = fromIntegral freq
      format'    = fromAudioFormat format
      channels'  = fromIntegral channels
      chunksize' = fromIntegral chunksize
  ret <- mixOpenAudio' freq' format' channels' chunksize'
  handleErrorI "openAudio" ret (const $ return ())

foreign import ccall unsafe "Mix_AllocateChannels"
  mixAllocateChannels' :: #{type int} -> IO #{type int}

allocateChannels :: Int -> IO ()
allocateChannels numchans = do
  ret <- mixAllocateChannels' (fromIntegral numchans)
  handleErrorI "allocateChannels" ret (const $ return ())

foreign import ccall unsafe "Mix_QuerySpec"
  mixQuerySpec' :: Ptr #{type int} -> Ptr #{type Uint16} -> Ptr #{type int} -> IO #{type int}

querySpec :: IO (Int, AudioFormat, Int)
querySpec =
  alloca $ \freq' ->
  alloca $ \format' ->
  alloca $ \channels' -> do
    ret <- mixQuerySpec' freq' format' channels'
    handleErrorI "querySpec" ret $ \_ -> do
      freq     <- fmap fromIntegral $ peek freq'
      format   <- fmap toAudioFormat $ peek format'
      channels <- fmap fromIntegral $ peek channels'
      return (freq, format, channels)

foreign import ccall unsafe "Mix_LoadWAV_RW"
  mixLoadWAVRW' :: Ptr RWopsStruct -> #{type int} -> IO (Ptr Chunk)

loadWAVRW :: RWops -> Bool -> IO Chunk
loadWAVRW rwops dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadWAVRW' rwops' (fromBool dofree) >>= peek

foreign import ccall unsafe "Mix_LoadMUS"
  mixLoadMUS' :: CString -> IO (Ptr MusicStruct)

loadMUS :: String -> IO Music
loadMUS fname =
  withCString fname $ \fname' ->
    mixLoadMUS' fname' >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_LoadMUS_RW"
  mixLoadMUSRW' :: Ptr RWopsStruct -> #{type int} -> IO (Ptr MusicStruct)

loadMUSRW :: RWops -> Bool -> IO Music
loadMUSRW rwops dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadMUSRW' rwops' (fromBool dofree) >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_LoadMUSType_RW"
  mixLoadMUSTypeRW' :: Ptr RWopsStruct -> #{type Mix_MusicType} -> #{type int} -> IO (Ptr MusicStruct)

loadMUSTypeRW :: RWops -> MusicType -> Bool -> IO Music
loadMUSTypeRW rwops mt dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadMUSTypeRW' rwops' (unwrapMusicType mt) (fromBool dofree) >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_QuickLoad_WAV"
  mixQuickLoadWAV' :: Ptr #{type Uint8} -> IO (Ptr Chunk)

quickLoadWav :: B.ByteString -> IO Chunk
quickLoadWav bs =
  let (bs'', _, _) = BI.toForeignPtr bs
  in withForeignPtr bs'' $ \bs' -> mixQuickLoadWAV' bs' >>= peek

foreign import ccall unsafe "Mix_QuickLoad_RAW"
  mixQuickLoadRAW' :: Ptr #{type Uint8} -> #{type Uint32} -> IO (Ptr Chunk)

quickLoadRAW :: B.ByteString -> IO Chunk
quickLoadRAW bs =
  let (bs'', _, len) = BI.toForeignPtr bs
  in withForeignPtr bs'' $ \bs' -> mixQuickLoadRAW' bs' (fromIntegral len) >>= peek

