pro make
  dir='/net/froz/exports/vol07/cecchgu/AVHRR/'
  ;20150404 MM : local copy for test
  dir='/net/froz/exports/vol07/cecchgu/AVHRR/'
  ;dirseawifs='/net/froz/exports/vol12/data/fapar_products/jrc/GLOBAL_PLC_0.005D/'
  ;file_jrc='SEA01_ORB01_19990101000000_19990101000000_L2_MUL_000006_900S900N1800W1800E_PLC_2170M_PRO.HDF'
  file_eric='AVH13C1.A1999150.N14.003.2010013190758.hdf'
  file_eric2='AVH09C1.A1999150.N14.003.2010013190758.hdf'

  ;pro read_data, dir, filename, name, array, info=INFO
  ;
  ;read_data, dir, file_eric,'NDVI', red_avhrr , /info
  ;read_data, dir, file_eric,'SREFL_CH2', nir_avhrr

  read_data, dir, file_eric,'QA', flag_avhrr1
  read_data, dir, file_eric2,'QA', flag_avhrr2

  ;read_data, dirseawifs, file_jrc,'Mean:BRF_Rec_R', red_seawifs
  ;read_data, dirseawifs, file_jrc,'Mean:BRF_Rec_N', nir_seawifs

  ;
  ; cloud
  ;
  for k =0, 16 do begin
    rr1=cgi_map_bitwise_flag(flag_avhrr1,k )
    ;rr11=cgi_map_bitwise_flag(flag_avhrr2,k)

    window,k, xsize=720, ysize=360, title=string(k)
    tvscl, reverse(congrid(rr1,720, 360),2)
    ;window,3, xsize=720, ysize=360, title='9 ndvi'
    ;tvscl, reverse(congrid(rr11,720, 360),2)

  endfor
  stop
end