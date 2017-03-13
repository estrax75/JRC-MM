function stdv, n, x,  m

  i=0;
  t = 0d;

  if (n lt 2 or n_elements(x) eq 0) then return, t;

  for i=0, n-1 do begin
    t += (x[i] - m)^2
  endfor
  return, sqrt(t/(n-1));

end
