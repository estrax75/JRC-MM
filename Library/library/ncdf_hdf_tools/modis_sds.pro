pro modis_sds,file_info
  ;---------------------------------------------------------------------
  ; filename  - for input HDF file name
  ; file_info - for printing output information
  ;---------------------------------------------------------------------

  ; -get the filename and assign a fileID
  filename=dialog_pickfile(filter='*.hdf')
  fileid = hdf_sd_start(filename, /read)

  ; -get information about the file:
  ;  -number of Scientific Data Sets (numsds)
  ;  -number of attributes (numatts) which are chunks of information
  hdf_sd_fileinfo, fileid, numsds, numatts

  ;--print this information to the screen
  print, ' '
  print, filename,fileid
  print, numsds,numatts
  print, ' '


  ;...  Save information in file_info:

  openw, fileinfoID, file_info, /get_lun

  printf, fileinfoID, ' '
  printf, fileinfoID, ' ',filename,fileid
  printf, fileinfoID, ' '
  printf, fileinfoID, ' # of SDSs =',numsds


  ;...  Gather information about all SDS:

  FOR isds = 0, numsds-1 DO BEGIN

    ; -get ID for each SDS, then get information about each one
    sdsid = hdf_sd_select(fileid,isds)
    hdf_sd_getinfo, sdsid, name=xname, hdf_type=xtype, ndim=xnumdim, $
      natt=xnumatts, dims=xdims

    ; -now print this information to the output file

    printf, fileinfoID, ' '
    printf, fileinfoID,'============================================================'
    printf, fileinfoID, ' iSDS:',isds,'           ',xname
    printf, fileinfoID,'============================================================'
    printf, fileinfoID, ' ndims:', xnumdim,'     hdf_type:  ',xtype
    printf, fileinfoID, ' dims: ', xdims
    printf, fileinfoID, ' natts:', xnumatts

    ; print, isds,xname,xnumdim,xnumatts,xtype, format='(I3,A52,I6,I5,A15)'

    ; -here we loop through all the attributes to print them
    if(xnumatts gt 0) then begin
      for iatt = 0, xnumatts-1 do begin
        hdf_sd_attrinfo, sdsid, iatt, name=attname, data=attdata
        printf, fileinfoID, 'IATT:',iatt,'  (',attname,')    ',attdata
      endfor
    endif

  ENDFOR

  free_lun, fileinfoID


  print, ' HDF file information saved in  ',file_info
  print, ' '

  hdf_sd_endaccess, sdsid
  hdf_sd_end, fileid

  RETURN
END