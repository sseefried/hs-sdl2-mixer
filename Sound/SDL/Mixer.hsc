#include "SDL_mixer.h"
module Sound.SDL.Mixer
  ( version
  ) where

-- | (Major, Minor, Patchlevel)
version :: (Int, Int, Int)
version = ( #{const SDL_MIXER_MAJOR_VERSION}
          , #{const SDL_MIXER_MINOR_VERSION}
          , #{const SDL_MIXER_PATCHLEVEL}
          )

