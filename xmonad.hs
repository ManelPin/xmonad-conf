-- import Control.Monad
--         ( forM
--         , replicateM
--         )

-- import Control.Exception
--         ( catch
--         -- , throw
--         -- , SomeException(SomeException)
--         )

import qualified Conf.XMonad
-- import qualified Conf.Applications

import Conf.Hooks.Log
        ( initBars
        -- , shutdownHandler
        )

-- import qualified Conf.Projects import qualified Conf.XMonad
import qualified Conf.Layouts.Nav2D
import qualified Conf.Bindings.Keys as Bindings.Keys
import qualified Conf.Bindings.Show as Bindings.Show

import qualified XMonad
import qualified XMonad.Actions.Navigation2D as Navigation2D
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
-- import qualified XMonad.Layout.IndependentScreens as IndependentScreens
import qualified XMonad.Util.NamedActions as NamedActions

-- import XMonad.Util.Run
--         ( unsafeSpawn
--         )

opts bars =
  -- Conf.Projects.dynamicProjects $
  Navigation2D.withNavigation2DConfig Conf.Layouts.Nav2D.nav2D $
  EwmhDesktops.ewmh $
  NamedActions.addDescrKeys' ((Bindings.Keys.modMask, XMonad.xK_F1), Bindings.Show.show) Bindings.Keys.keys $
  Conf.XMonad.xmonad bars

main
  = do
    bars <- initBars
    XMonad.xmonad $ opts bars
    -- (XMonad.xmonad $ opts bars) `catch` (shutdownHandler bars)
