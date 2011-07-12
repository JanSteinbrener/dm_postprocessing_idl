;+
; NAME:
;    DM_DO_ADI_FILTER
;
; PURPOSE:
;
;    This function creates a filter from the adi_array by smoothing
;    the unknonw regions and applies this filter to the FFT of
;    itn_array.  Width is used by the smoothing function and is
;    defaulted as a 10th of the array size.  The depth default is 0.5.
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
;    dm_do_adi_filter, itn_array, width, depth
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
;    width:  is set to 50th the size of itn_array if not given
;    depth:  is set to 0.5 if not given
;
;
; INPUT KEYWORDS:
;
;    none
;
;
; MODIFICATION HISTORY:
;    2009-08-17 JN: written
;-
FUNCTION dm_do_adi_filter, adi_array, itn_array, width, depth, $
                           is_data_centered=is_data_centered

  svec = size(itn_array)
  nx = svec[1]
  ny = svec[2]
  
  data = bh_sfft(itn_array, /double, /preserve_power)
  ;window, /f, xsize=600, ysize=600
  ;tvscl, congrid(alog10(data+0.00001),600,600)
  
  IF n_elements(width) EQ 0 THEN width = nx/50.
  IF n_elements(depth) EQ 0 THEN depth = 0.5
  
  nonzero = WHERE(adi_array GT 0., count)
  filter = adi_array + depth
  if count ne 0 then filter[nonzero] = 1.0
  if not keyword_set(is_data_centered) then begin
     filter = SMOOTH(shift(filter, nx/2,ny/2), width)
  endif else filter = SMOOTH(filter, width)
  
  ;window, /f, xsize=600, ysize=600
  ;tvscl, congrid(alog10(data*filter+0.00001),600,600)
  new_itn_array = bh_sfft((filter*data), /double, /preserve_power)
  ;window, /f, xsize=600, ysize=600
  ;tvscl, congrid(alog10(filter+0.00001),600,600)

  RETURN, new_itn_array

END
