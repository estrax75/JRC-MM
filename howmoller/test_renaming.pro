;launch_BRF_AVHRR_Howmuller, 1, 2003, 'nc'
pro testrenaming
  COMMON singleTons, ST_utils, ST_operator, ST_fileSystem

  declareSingleTons

  inputDir='/space3/storage/products/results/BRFs'
  aa=file_search(inputDir, '*.hdf', /FULLY)
  ;file_move,

end