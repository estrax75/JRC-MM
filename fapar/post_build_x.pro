; Resolve all routines (skip built-in ENVI routines)
RESOLVE_ALL, /CONTINUE_ON_ERROR, SKIP_ROUTINES='envi'
.COMPILE '/home/mariomi/IDLWorkspace85/Library/library/objects/FileSystem__define.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/Library/library/jrc_core/ltdr/equation/getsensorcoeffs.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/Library/library/jrc_core/ltdr/equation/getNOAAcoeff.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/fapar/launch_fapar_daily_SPP.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/fapar/launch_fapar_daily.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/Library/library/jrc_core/ltdr/rectified.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/Library/library/jrc_core/ltdr/fapar.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/fapar/main/doTimeCompositeFapar_split.pro'
.COMPILE '/home/mariomi/IDLWorkspace85/fapar/tc/sm_call_mean_3_unc.pro'
SAVE, /ROUTINES, FILENAME='/home/mariomi/shared/fapar_test/launch_fapar_last.sav', /VERBOSE
