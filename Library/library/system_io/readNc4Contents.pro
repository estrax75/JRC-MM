
; docformat:'rst'

;+
; Function to read netcdf-4 data.  
;
; :version: 1
;
; :Author:
;			
;   Frederic Melin (European Commission/JRC/IES)
;
; :Categories:
; 
;   reading
;   
; :Description:
; 
;   reads Level-3 data in netcdf-4 format (typically NASA L3 SMI data), and returns a structure.
;	by defining 'start' and 'count', only a slab of the data is read.
;   
;   Contains::
;   
;     vname: name of the variable
;	  data: data (2D) 
;	  scale: scaling factor (if available, default = 1)
;	  offset: intercept (if available, default = 0)
;	  badv: bad value (if available, default = -32767.)
;     
; :Requires:
; 
;   IDL
;   
; :Uses:
; 
;   strdata = read_nc4_dataset(ifile,vname,start=[selem,sline],count=[nx,ny])
;			or:
;   strdata = read_nc4_dataset(ifile,vname)
;   
; :Params:
; 
;   ifile: in, type=string
;			
;      input file
;			
;   vname: in, type=string
;   
;     name of the variable to be read (for example: "/chlor_a")
;			
;   start: in, type integer, optional (default: [0,0])
;			
;	    vector with start element and line;
;
;	count: in, type integer, optional (ignored if 'start' absent)
;			
;	    vector with number of elements and lines to be read;
;     
;              
; :Keywords:
;   
;-

; MM extended input parameters
;FUNCTION readNc4Contents,file,vname,start=start,count=count
FUNCTION readNc4Contents, infilenames, inputVarList, outputVarList, conv_functions, $
  tempDir, roiName, ignoreValue, NOTFOUND=NOTFOUND, MEMORY=MEMORY, targetCropInfo=targetCropInfo, start=start, count=count

 resStruct=read_nc4_dataset(infilenames, inputVarList, start=start, count=count)
 return, resStruct

END
