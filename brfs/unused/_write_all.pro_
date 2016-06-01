pro write_all, fileName, data, dir=dir, refDims=refDims

  if n_elements(dir) eq 1 then begin
    sepSign=strpos(dir, path_sep(), /REVERSE_SEARCH)
    if sepSign ne strlen(dir)-1 then dir=dir+path_sep()
    file=dir+new_file
  endif
  
  ;stop
  sdid_outfile1 = HDF_SD_START(file, /CREATE)

  ; to Add some glob attrib
  ;ncdf_write_global_attribute, file_id, attname, attvalue

  sdid_fpar = HDF_SD_CREATE(sdid_outfile1, 'FAPAR', [a[1],a[2]], /float)
  sdid_uncert = HDF_SD_CREATE(sdid_outfile1, 'Sigma FAPAR', [a(1),a(2)], /float)
  sdid_red = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED RED', [a(1),a(2)], /float)
  sdid_uncert_red = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED RED', [a(1),a(2)], /float)
  sdid_nir = HDF_SD_CREATE(sdid_outfile1, 'RECTIFIED NIR', [a(1),a(2)], /float)
  sdid_uncert_nir = HDF_SD_CREATE(sdid_outfile1, 'Sigma RECTIFIED NIR', [a(1),a(2)], /float)
  sdid_flag = HDF_SD_CREATE(sdid_outfile1, 'FLAG', [a(1),a(2)], /byte)
  sdid_redtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC RED', [a(1),a(2)], /float)
  sdid_nirtoc = HDF_SD_CREATE(sdid_outfile1, 'BRF TOC NIR', [a(1),a(2)], /float)
  sdid_qa =  HDF_SD_CREATE(sdid_outfile1, 'JRC QA', [a(1),a(2)], /byte)

  ; MM 20150405
  sdid_sza =  HDF_SD_CREATE(sdid_outfile1, 'SZA', [a(1),a(2)], /float)
  sdid_vza =  HDF_SD_CREATE(sdid_outfile1, 'VZA', [a(1),a(2)], /float)
  sdid_saa =  HDF_SD_CREATE(sdid_outfile1, 'SAA', [a(1),a(2)], /float)
  sdid_vaa =  HDF_SD_CREATE(sdid_outfile1, 'VAA', [a(1),a(2)], /float)
  ; END

  HDF_SD_ADDDATA, sdid_fpar, output.fpar
  HDF_SD_ADDDATA, sdid_uncert, output.sigma
  HDF_SD_ADDDATA, sdid_red, output.red
  HDF_SD_ADDDATA, sdid_uncert_red, output.sigma_red
  HDF_SD_ADDDATA, sdid_nir, output.nir
  HDF_SD_ADDDATA, sdid_uncert_nir, output.sigma_nir
  HDF_SD_ADDDATA, sdid_flag, output.flag
  HDF_SD_ADDDATA, sdid_redtoc, reflectance(*,*,0)
  HDF_SD_ADDDATA, sdid_nirtoc, reflectance(*,*,1)
  HDF_SD_ADDDATA, sdid_qa, MASK_avhrr

  ; MM 20150405
  HDF_SD_ADDDATA, sdid_sza, sza
  HDF_SD_ADDDATA, sdid_vza, vza
  HDF_SD_ADDDATA, sdid_saa, saa
  HDF_SD_ADDDATA, sdid_vaa, vaa
  ; END
  HDF_SD_END, sdid_outfile1
end