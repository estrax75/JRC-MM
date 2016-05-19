FUNCTION unzipit, spawnString, fileName, outputDir

 utils=obj_new('utility')
 fs=obj_new('FileSystem', /STAND_ALONE)
 
 runSpawnString=utils->strreplace(spawnString, '%OUTPUTDIR%', outputDir)
 runSpawnString=utils->strreplace(runSpawnString, '%INPUTFULLFILE%', filename)
 lastChar=rstrpos(fileName, '.')
 firstChar=rstrpos(fileName, path_sep())+1
 unzipFileName=strmid(fileName, firstChar, lastChar-firstChar)
 if fs->isOSWin() then HIDE=1 else HIDE=0
 obj_destroy, utils
 obj_destroy, fs

 if (HIDE) then spawn, runSpawnString, HIDE=HIDE else spawn, runSpawnString
 return, unzipFileName

END