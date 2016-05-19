PRO delIDLVar, varName 

  varName=0
  fake=ptr_new(varName, /NO_COPY)
  ptr_free,fake  

END
