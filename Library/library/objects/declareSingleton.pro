PRO declareSingleTons, spptemplateFile

  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  if ~obj_valid(ST_utils) then ST_utils=obj_new('Utility')
  if ~obj_valid(ST_operator) then ST_operator=obj_new('GenericOperator')
  if ~obj_valid(ST_fileSystem) or n_elements(spptemplateFile) eq 1 then ST_fileSystem=obj_new('FileSystem', /STAND, spptemplateFile=spptemplateFile)
  
END

;****************************************************************************************
