function rmse, mata ,matb

  ; mata      matrix
  ; matb      matrix too, at least same number type  as matrix
  ; out     rmse

  mat1=reform(mata)
  mat2=reform(matb)


  s1=size(mat1)
  s2=size(mat2)
  n1=n_elements(mat1)
  n2=n_elements(mat2)


  doit= s1(0) ne  s2(0) and n1 eq n2
  if doit then begin
    print,'Number of Dimensions do not agree, but number of elements'
    mat1=reform(mat1,n1)
    mat2=reform(mat1,n2)
  endif





  doit= s1(0) eq  s2(0) and n1 eq n2
  if doit then begin
    doit= total( s1(1:s1(0)) ne s2(1:s2(0))) and  (n1 eq n2)
    if doit then begin
      print,'Dimensions do not agree, but number of elements'
      mat1=reform(mat1,n1)
      mat2=reform(mat1,n2)
    endif
  endif



  if  n1 ne n2 then begin
    print,'number of elements do not agree'
    return,0
  endif




  out= sqrt(total((mat1-mat2)^2)/n1)

  return,out

end

