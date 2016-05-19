;+NAME/ONE LINE DESCRIPTION OF ROUTINE:
;    GET_VDS_IN_VG returns names of Vdatas within a Vgroup
;
FUNCTION get_vds_in_vg, filename, vg_name, vd_class

; This function returns the names of all Vdatas of a specified class
; within a specified Vgroup.


; Open file
; ---------
fileid = HDF_OPEN(filename, /READ)


; Loop through Vgroups in file until the desired Vgroup is found
; --------------------------------------------------------------
name = ''
vg_ref = -1
WHILE (name NE vg_name) DO BEGIN
	vg_ref = HDF_VG_GETID(fileid, vg_ref)

	; If not found then bail
	IF (vg_ref EQ -1) THEN BEGIN
		PRINT, 'Vgroup: "', vg_name, '" not found.'
		HDF_CLOSE, fileid
		RETURN, ''	
	ENDIF

	vg_id = HDF_VG_ATTACH(fileid, vg_ref, /READ)

	HDF_VG_GETINFO, vg_id, NAME=name
;	print,name
ENDWHILE


; Get tags and reference numbers in Vgroup
; ----------------------------------------
HDF_VG_GETTRS, vg_id, tags, refs
n = N_ELEMENTS(tags)
HDF_VG_DETACH, vg_id


; Search Vdata names in Vgroup
; ----------------------------
vd_name = ''
FOR i=0,n-1 DO BEGIN
	IF (tags[i] EQ 1962) THEN BEGIN
		vdata_id = HDF_VD_ATTACH(fileid, refs[i])
		HDF_VD_GET, vdata_id, NAME=name, CLASS=class
		IF (class EQ vd_class) THEN vd_name = [vd_name, name]
		HDF_VD_DETACH, vdata_id
	ENDIF
ENDFOR

IF (N_ELEMENTS(vd_name) EQ 1) THEN PRINT, 'No Vdatas Found' $
ELSE vd_name = vd_name[1:*]

HDF_CLOSE, fileid

RETURN, vd_name
END
