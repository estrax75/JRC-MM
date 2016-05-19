function sortArrayByValue, array, subscribeIndexes=subscribeIndexes, NAN=NAN, FOUND=FOUND, DECREASING=DECREASING, ASCENDING=ASCENDING, outSubscribe=outSubscribe

  dims=size(array, /DIM)
  scores=fix(array)
  for i=0, dims[1]-1 do scores[*,i]=sort(array[*,i])
  idx=intarr(dims[0])
  for i=0, dims[0]-1 do idx[i]=total(where(scores eq i))
  outSubscribe=sort(idx)
  if keyword_set(DECREASING) then begin
    outSubscribe=reverse(outSubscribe)
  endif
  return, array[outSubscribe, *]
  
end
