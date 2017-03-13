function getMask, fapar, red, nir, jrc_flag, VEGETATION=VEGETATION, SOIL=SOIL

  if keyword_set(VEGETATION) then begin
  endif
  if keyword_set(SOIL) then begin
    validMask=finite(data_day_split(t).fapar(*,*)) and finite(data_day_split(t).red(*,*)) and finite(data_day_split(t).nir(*,*))
    goodIndexes=where(validMask eq 1)
    ;create Soil mask
    idxMaskSoil=where(data_day_split(t).fapar[goodIndexes] eq 0.0 and $
      data_day_split(t).red[goodIndexes] gt 0.0 and data_day_split(t).red[goodIndexes] lt 1.0 and $
      data_day_split(t).nir[goodIndexes] gt 0.0 and data_day_split(t).nir[goodIndexes] lt 1.0)
    validMaskSoil=validMask*0
    validMaskSoil[goodIndexes[idxMaskSoil]]=1
    validMaskSoil=validMask*validMaskSoil
    idx_maskSoil = where(validMaskSoil eq 1 and (data_day_split(t).jrc_flag(*,*) eq 4 or data_day_split(t).jrc_flag(*,*) eq 5), countSoil)
    vMask=validMaskSoil
  endif
  return, vMask

end