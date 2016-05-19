function buildLogValues, low, high, step=step

 if n_elements(step) ne 1 then logStep=1 else logStep=step
 logValues=[0]
 low=float(low)
 for i=low, high, logStep do logValues=[logValues, 10^float(i)]
 if logValues[n_elements(logValues)-1] ne (10^high) then logValues=[logValues, 10^float(i)]
 logValues=logValues[1:*]
 return, logValues 

end