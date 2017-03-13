; Resolve all routines (skip built-in ENVI routines)
RESOLVE_ALL, /CONTINUE_ON_ERROR, SKIP_ROUTINES='envi'
;if strupcase(!VERSION.OS_FAMILY) eq strupcase('windows') then @post_build_win
@post_build_win
;if strupcase(!VERSION.OS_FAMILY) eq strupcase('unix') then @post_build_x
;@post_build_x

; Create the project save file
message, 'remember to rename sav file!!!'