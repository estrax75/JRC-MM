PRO envi_setup_head_oxy, FNAME=FNAME, NS=ns, NL=nl, DATA_IGNORE_VALUE=DATA_IGNORE_VALUE, $
    NB=NB, DATA_TYPE=DATA_TYPE, FILE_TYPE=FILE_TYPE, INTERLEAVE=INTERLEAVE, MAP_INFO=MAP_INFO, WRITE=WRITE,$
    OPEN=OPEN, BNAMES=BNAMES, r_fid=r_fid
    
  fs=obj_new('FileSystem', /STAND)
  newFName=fs->removeFileExtension(FNAME)
  fs->removeEnviHeaderFile, newFName
  MAP_INFO.nl=nl
  MAP_INFO.ns=ns
  envi_setup_head,  $
    R_FID=R_FID, $
    FNAME=newFName,$
    NS=ns,$
    NL=nl, $
    DATA_IGNORE_VALUE=DATA_IGNORE_VALUE, $
    NB=NB, $
    DATA_TYPE=DATA_TYPE, $
    FILE_TYPE=FILE_TYPE, $
    INTERLEAVE=INTERLEAVE, $
    MAP_INFO=MAP_INFO, $
    WRITE=WRITE,$
    OPEN=OPEN, $
    BNAMES=BNAMES
  fs->correctEnviHeaderFileName, FNAME
  obj_destroy, fs
  
END
