FUNCTION convertValues, data, conversionFormula, ignoreValue=ignoreValue

 ;util=obj_new('Utility')
 if n_elements(conversionFormula) eq 0 then return, data
 if conversionFormula eq 'N/A' or conversionFormula eq '' then return, data
 if n_elements(ignoreValue) eq 1 then idx=where(data eq ignoreValue, count)
 res=execute('resData='+conversionFormula)
 if count ne 0 then resData[idx]=ignoreValue
 ;obj_destroy, util 
 return, resData

END
