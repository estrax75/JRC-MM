function convertDataType, sourceValue, destDataType, UNSIGNED=UNSIGNED

 case destDataType of
      1: res=byte(fix(sourceValue))
      5: res=double(sourceValue)
      4: res=float(sourceValue)
      2: res=fix(sourceValue)
      3: res=long(sourceValue)
      7: res=string(sourceValue)
      else:message, 'conversion not allowed'
 endcase
 if keyword_set(UNSIGNED) and destDataType eq 1 then res=not(res)-1
 return, res
 
end