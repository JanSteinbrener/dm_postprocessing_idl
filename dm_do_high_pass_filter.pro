;+
; NAME:
;    DM_HIGH_PASS_FILTER
;
; PURPOSE:
;
;    This function performs a doughnut shaped high pass filter 
;    on the FFT of itn_array as done in Chapman et al. 2006.  Sigma is
;    defaulted as a 10th of the array size and the depth default is 0.5.
;
; AUTHOR:
;
;    Johanna Nelson
;    jnelson@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;    Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;    dm_do_high_pass_filter, itn_array, sigma, depth
;
; RETURN VALUE:
;
;    new_itn_array
;
;
; INPUT PARAMETERS:
;
;    itn_array
;
; INPUT/OUTPUT PARAMETERS:
;
;    sigma:  is set to 10th the size of itn_array if not given
;    depth:  is set to 0.5 if not given
;
;
; INPUT KEYWORDS:
;
;    none
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2010-01-20 JN: fix flip across the diagonal by using keyword
;    "/inverse" to bh_sfft
;-
FUNCTION dm_do_high_pass_filter, itn_array, sigma, depth

  svec = size(itn_array)
  nx = svec[1]
  ny = svec[2]
  
  data = BH_SFFT(itn_array, /DOUBLE, /PRESERVE_POWER)
  radius = SHIFT(DIST(nx, ny), nx/2, ny/2)
  if N_ELEMENTS(sigma) eq 0 then sigma = nx/10
  if N_ELEMENTS(depth) eq 0 then depth = 0.5
  
  filter = depth+((1.-depth)*(radius/sigma)^4)*EXP(2-radius^2/(sigma^2/2))
  sigma_index = WHERE(radius GE sigma, count)
  if count ne 0 then filter[sigma_index] = 1.0  ;; outside sigma filter is 1
  
  new_itn_array = BH_SFFT((filter*data), /DOUBLE, /PRESERVE_POWER, /INVERSE)

  RETURN, new_itn_array

END
