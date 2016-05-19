PRO get_sds_names, file, sds_names

sds_names = ''

sd_id = HDF_SD_START(file, /READ)
HDF_SD_FILEINFO, sd_id, n_datasets,n_attr

FOR i=0,n_datasets-1 DO BEGIN
	sds_id = HDF_SD_SELECT(sd_id,i)

	HDF_SD_GETINFO, sds_id, name=name

	sds_names = [sds_names, name]

	HDF_SD_ENDACCESS, sds_id     

ENDFOR

HDF_SD_END, sd_id

sds_names = sds_names[1:*]

RETURN
END

