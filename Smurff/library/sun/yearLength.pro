function yearLength, year
  ;% Function to calculate length of year
  ;%
  ;% Input:
  ;%  year: yyyy
  ;%
  ;% Output:
  ;%  year_len - year length in days
  
  return, 365+((year mod 4 eq 0) and ( (~(year mod 100 eq 0)) or (year mod 400 eq 0)))
  
end