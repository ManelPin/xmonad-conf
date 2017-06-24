import qualified Conf.Applications
import qualified Conf.Projects
import qualified Conf.XMonad
import qualified Conf.Layouts.Nav2D

import qualified Conf.Bindings.Keys as Bindings.Keys
import qualified Conf.Bindings.Show as Bindings.Show

import qualified XMonad

import qualified XMonad.Actions.Navigation2D as Navigation2D

import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops

import qualified XMonad.Util.NamedActions as NamedActions
import qualified XMonad.Util.Run as Run

main = do
  xmobarProc <- Run.spawnPipe
    Conf.Applications.statusBar

  XMonad.xmonad $
    Conf.Projects.dynamicProjects $
    Navigation2D.withNavigation2DConfig Conf.Layouts.Nav2D.nav2D $
    EwmhDesktops.ewmh $
    NamedActions.addDescrKeys'
      ((Bindings.Keys.modMask, XMonad.xK_F1), Bindings.Show.show) Bindings.Keys.keys $
    Conf.XMonad.xmonad xmobarProc
