function getHowMollerSavFile, dir, sourceParameter, parameter, year, month, TC_TYPE, noaanumber

  return, dir+parameter+'_'+year+month+$
    '_'+TC_TYPE+'_avhrr_'+sourceParameter+noaanumber+'.sav'

end