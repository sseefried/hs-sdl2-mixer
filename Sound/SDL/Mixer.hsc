#include "SDL_mixer.h"
module Sound.SDL.Mixer
  ( version
  , initFLAC
  , initMOD
  , initMODPLUG
  , initMP3
  , initOGG
  , initFLUIDSYNTH
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
  , getNumChunkDecoders
  , getChunkDecoder
  , getNumMusicDecoders
  , getMusicDecoder
  , getMusicType
  , setPanning
  , setPosition
  , setDistance
  , setReverseStereo
  , reserveChannels
  , groupChannel
  , groupChannels
  , groupAvailable
  , groupCount
  , groupOldest
  , groupNewer
  , playChannelTimes
  , playMusic
  , fadeInMusic
  , fadeInMusicPos
  , fadeInChannelTimed
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
    mixLoadMUSTypeRW' rwops' (musicTypeToConstant mt) (fromBool dofree) >>= mkFinalizedMusic

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

foreign import ccall unsafe "Mix_GetNumChunkDecoders"
  mixGetNumChunkDecoders' :: IO #{type int}

getNumChunkDecoders :: IO Int
getNumChunkDecoders = mixGetNumChunkDecoders' >>= return . fromIntegral

foreign import ccall unsafe "Mix_GetChunkDecoder"
  mixGetChunkDecoder' :: #{type int} -> IO CString

getChunkDecoder :: Int -> IO String
getChunkDecoder index =
  mixGetChunkDecoder' (fromIntegral index) >>= peekCString

foreign import ccall unsafe "Mix_GetNumMusicDecoders"
  mixGetNumMusicDecoders' :: IO #{type int}

getNumMusicDecoders :: IO Int
getNumMusicDecoders = mixGetNumMusicDecoders' >>= return . fromIntegral

foreign import ccall unsafe "Mix_GetMusicDecoder"
  mixGetMusicDecoder' :: #{type int} -> IO CString

getMusicDecoder :: Int -> IO String
getMusicDecoder index =
  mixGetMusicDecoder' (fromIntegral index) >>= peekCString

foreign import ccall unsafe "Mix_GetMusicType"
  mixGetMusicType' :: Ptr MusicStruct -> IO #{type Mix_MusicType}

getMusicType :: Music -> IO MusicType
getMusicType music =
  withForeignPtr music $ \music' ->
    mixGetMusicType' music' >>= return . constantToMusicType

musicTypeToConstant :: MusicType -> #{type Mix_MusicType}
musicTypeToConstant NONE = #{const MUS_NONE}
musicTypeToConstant CMD = #{const MUS_CMD}
musicTypeToConstant WAV = #{const MUS_WAV}
musicTypeToConstant MOD = #{const MUS_MOD}
musicTypeToConstant MID = #{const MUS_MID}
musicTypeToConstant OGG = #{const MUS_OGG}
musicTypeToConstant MP3 = #{const MUS_MP3}
musicTypeToConstant MP3_MAD = #{const MUS_MP3_MAD}
musicTypeToConstant FLAC = #{const MUS_FLAC}
musicTypeToConstant MODPLUG = #{const MUS_MODPLUG}

constantToMusicType :: #{type Mix_MusicType} -> MusicType
constantToMusicType #{const MUS_NONE} = NONE
constantToMusicType #{const MUS_CMD} = CMD
constantToMusicType #{const MUS_WAV} = WAV
constantToMusicType #{const MUS_MOD} = MOD
constantToMusicType #{const MUS_MID} = MID
constantToMusicType #{const MUS_OGG} = OGG
constantToMusicType #{const MUS_MP3} = MP3
constantToMusicType #{const MUS_MP3_MAD} = MP3_MAD
constantToMusicType #{const MUS_FLAC} = FLAC
constantToMusicType #{const MUS_MODPLUG} = MODPLUG
constantToMusicType _ = error "invalid music type"

{- TODO
Mix_SetPostMix
Mix_HoolMusic
Mix_HookMusicFinished
Mix_GetMusicHookData
Mix_ChannelFinished
Mix_RegisterEffect
Mix_UnregisterEffect
Mix_UnregisterAllEffects
--}

foreign import ccall unsafe "Mix_SetPanning"
  mixSetPanning' :: #{type int} -> #{type Uint8} -> #{type Uint8} -> IO #{type int}

setPanning :: Int -> Int -> Int -> IO ()
setPanning channel left right = do
  let channel' = fromIntegral channel
      left'    = fromIntegral left
      right'   = fromIntegral right
  ret <- mixSetPanning' channel' left' right'
  handleErrorI "setPanning" ret $ (const $ return ())

foreign import ccall unsafe "Mix_SetPosition"
  mixSetPosition' :: #{type int} -> #{type Sint16} -> #{type Uint8} -> IO #{type int}

setPosition :: Int -> Int -> Int -> IO ()
setPosition channel angle distance = do
  let channel'  = fromIntegral channel
      angle'    = fromIntegral angle
      distance' = fromIntegral distance
  ret <- mixSetPosition' channel' angle' distance'
  handleErrorI "setPosition" ret $ (const $ return ())

foreign import ccall unsafe "Mix_SetDistance"
  mixSetDistance' :: #{type int} -> #{type Uint8} -> IO #{type int}

setDistance :: Int -> Int -> IO ()
setDistance channel distance = do
  let channel'  = fromIntegral channel
      distance' = fromIntegral distance
  ret <- mixSetDistance' channel' distance'
  handleErrorI "setDistance" ret $ (const $ return ())

foreign import ccall unsafe "Mix_SetReverseStereo"
  mixSetReverseStereo' :: #{type int} -> #{type int} -> IO #{type int}

setReverseStereo :: Int -> Bool -> IO ()
setReverseStereo channel doflip = do
  let channel' = fromIntegral channel
  ret <- mixSetReverseStereo' channel' (fromBool doflip)
  handleErrorI "setReverseStereo" ret $ (const $ return ())

foreign import ccall unsafe "Mix_ReserveChannels"
  mixReserveChannels' :: #{type int} -> IO #{type int}

reserveChannels :: Int -> IO ()
reserveChannels num = do
  ret <- mixReserveChannels' (fromIntegral num)
  handleErrorI "reserveChannels" ret $ (const $ return ())

foreign import ccall unsafe "Mix_GroupChannel"
  mixGroupChannel' :: #{type int} -> #{type int} -> IO #{type int}

groupChannel :: Int -> Int -> IO Bool
groupChannel which tag =
  mixGroupChannel' (fromIntegral which) (fromIntegral tag) >>= return . toBool

foreign import ccall unsafe "Mix_GroupChannels"
  mixGroupChannels' :: #{type int} -> #{type int} -> #{type int} -> IO #{type int}

groupChannels :: Int -> Int -> Int -> IO Bool
groupChannels from to tag =
  let from' = fromIntegral from
      to'   = fromIntegral to
      tag'  = fromIntegral tag
  in mixGroupChannels' from' to' tag' >>= return . toBool

foreign import ccall unsafe "Mix_GroupAvailable"
  mixGroupAvailable' :: #{type int} -> IO #{type int}

groupAvailable :: Int -> IO Int
groupAvailable tag = mixGroupAvailable' (fromIntegral tag) >>= return . fromIntegral

foreign import ccall unsafe "Mix_GroupCount"
  mixGroupCount' :: #{type int} -> IO #{type int}

groupCount :: Int -> IO Int
groupCount tag = mixGroupCount' (fromIntegral tag) >>= return . fromIntegral

foreign import ccall unsafe "Mix_GroupOldest"
  mixGroupOldest' :: #{type int} -> IO #{type int}

groupOldest :: Int -> IO Int
groupOldest tag =
  mixGroupOldest' (fromIntegral tag) >>= return . fromIntegral

foreign import ccall unsafe "Mix_GroupNewer"
  mixGroupNewer' :: #{type int} -> IO #{type int}

groupNewer :: Int -> IO Int
groupNewer tag = mixGroupNewer' (fromIntegral tag) >>= return . fromIntegral

foreign import ccall unsafe "Mix_PlayChannelTimed"
  mixPlayChannelTimes' :: #{type int} -> Ptr Chunk -> #{type int} -> #{type int} -> IO #{type int}

playChannelTimes :: Int -> Chunk -> Int -> Int -> IO Int
playChannelTimes channel chunk loops ticks =
  let channel' = fromIntegral channel
      loops'   = fromIntegral loops
      ticks'   = fromIntegral ticks
  in with chunk $ \chunk' ->
       mixPlayChannelTimes' channel' chunk' loops' ticks' >>= return . fromIntegral

foreign import ccall unsafe "Mix_PlayMusic"
  mixPlayMusic' :: Ptr MusicStruct -> #{type int} -> IO #{type int}

playMusic :: Music -> Int -> IO ()
playMusic music loops =
  withForeignPtr music $ \music' -> do
    ret <- mixPlayMusic' music' (fromIntegral loops)
    handleErrorI "playMusic" ret (const $ return ())

foreign import ccall unsafe "Mix_FadeInMusic"
  mixFadeInMusic' :: Ptr MusicStruct -> #{type int} -> #{type int} -> IO #{type int}

fadeInMusic :: Music -> Int -> Int -> IO ()
fadeInMusic music loops ms =
  withForeignPtr music $ \music' -> do
    ret <- mixFadeInMusic' music' (fromIntegral loops) (fromIntegral ms)
    handleErrorI "fadeInMusic" ret (const $ return ())

foreign import ccall unsafe "Mix_FadeInMusicPos"
  mixFadeInMusicPos' :: Ptr MusicStruct -> #{type int} -> #{type int} -> #{type double} -> IO #{type int}

fadeInMusicPos :: Music -> Int -> Int -> Double -> IO ()
fadeInMusicPos music loops ms pos =
  withForeignPtr music $ \music' -> do
    ret <- mixFadeInMusicPos' music' (fromIntegral loops) (fromIntegral ms) pos
    handleErrorI "fadeInMusicPos" ret (const $ return ())

foreign import ccall unsafe "Mix_FadeInChannelTimed"
  mixFadeInChannelTimed' :: #{type int} -> Ptr Chunk -> #{type int} -> #{type int} -> #{type int} -> IO #{type int}

fadeInChannelTimed :: Int -> Chunk -> Int -> Int -> Int -> IO ()
fadeInChannelTimed channel chunk loops ms ticks =
  with chunk $ \chunk' -> do
    let channel' = fromIntegral channel
        loops'   = fromIntegral loops
        ms'      = fromIntegral ms
        ticks'   = fromIntegral ticks
    ret <- mixFadeInChannelTimed' channel' chunk' loops' ms' ticks'
    handleErrorI "fadeInChannelTimed" ret (const $ return ())

