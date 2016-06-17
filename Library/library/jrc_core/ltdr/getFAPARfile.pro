function getFAPARFile, noaanumber, year, month, startDay, endDay, LTYPE

  return, 'AVHRR_NOA'+noaanumber+'_'+year+month+startDay+'000000_'+year+month+endDay+'000000_'+LTYPE+'_MUL_000001_900S900N1800W1800E_PLC_0005D_PRO.NC'

end

