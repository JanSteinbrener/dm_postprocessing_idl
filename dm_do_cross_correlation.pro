FUNCTION dm_do_cross_correlation, array1, array2, shift_value

  ;; Inverse FT of the FT of the complex conjugate of array1 times FT
  ;; of array2

  fourier1 = BH_SFFT(array1, /PRESERVE_POWER, /DOUBLE)
  fourier2 = BH_SFFT(array2, /PRESERVE_POWER, /DOUBLE)
 
  ;; Gaussian high-pass filter to eliminate the effect of the support
  ;; constraint
  ;; Remove any negatives from intensities
  array_size = size(fourier1,/dimensions)
  if N_ELEMENTS(array_size) eq 2 then begin
     radius = shift(dist(array_size[0],array_size[1]),array_size[0]/2, $
                    array_size[1]/2)
  endif else begin
     radius = FLTARR(array_size)
     for i=0L,array_size[0]-1 do begin
        for j=0L,array_size[1]-1 do begin
           for k=0L,array_size[2]-1 do begin
              radius[i,j,k] = SQRT((ABS(i-array_size[0]/2))^2 + $
                                   (ABS(j-array_size[1]/2))^2 + $
                                   (ABS(k-array_size[2]/2))^2)
           endfor
        endfor
     endfor
  endelse

  sigma=array_size[0]/10
  depth = 0.5
  filter = depth+((1.-depth)*(radius/sigma)^4)*EXP(2-radius^2/(sigma^2/2))
  sigma_index = WHERE(radius GE sigma, count)
  if count ne 0 then filter[sigma_index] = 1.0  ;; outside sigma filter is 1

  cross_correlation = BH_SFFT(CONJ(fourier1*filter)*(fourier2*filter),$
                              /PRESERVE_POWER, /DOUBLE, /INVERSE)
  ;cross_correlation = BH_SFFT(CONJ(fourier1/abs(fourier1))*$
  ;                            (fourier2/abs(fourier2)),/PRESERVE_POWER,$
   ;                           /DOUBLE, /INVERSE)

  max_cross = MAX(cross_correlation, max_index)
  max_index = ARRAY_INDICES(cross_correlation, max_index)
  shift_value = array_size/2. - max_index
  ;print, 'Crosscorrelation gives shift of ', BYTE(shift_value)

  ;; calculates sub-pixel shifts by fitting to a Gaussian
  gauss_fit = gauss2dfit(abs(cross_correlation), coefficients)
  shift_value= array_size/2.- [coefficients[4],coefficients[5]]

  return, cross_correlation

END
