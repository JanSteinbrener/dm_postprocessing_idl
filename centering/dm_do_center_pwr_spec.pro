;+
; NAME:
;    DM_DO_CENTER_PWR_SPEC
;
; PURPOSE:
;
;    This function calculates the slope of a the power spectrum of a
;    given adi_array.
;
; AUTHOR:
;
;    Johanna Nelson
;    jnelson@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;    Reconstruction algorithms: dm_recon/centering
;
;
; CALLING SEQUENCE:
;
;    slope = dm_do_center_pwr_spec(diff_intensity
;       [,step_size_pixels = step_size_pixels])
;
; RETURN VALUE:
;
;    slope
;
;
; INPUT PARAMETERS:
;
;    adi_array
;
;
; INPUT KEYWORDS:
;
;    step_size_pixels:  Width of ring in which intensity is averaged.
;       If not set, default is used.
;
;
; MODIFICATION HISTORY:
;    2009-01-19 JN: written
;-

FUNCTION dm_do_center_pwr_spec, diff_intensity, $
                                step_size_pixels=step_size_pixels

  nx = n_elements(diff_intensity[*,0])
  ny = n_elements(diff_intensity[0,*])

  power_spec_struct = dm_do_power_spectrum(diff_intensity, $
                                           step_size_pixels=step_size_pixels)
  power_spec = power_spec_struct.power_spec/2.0
  f_spatial_pixels = power_spec_struct.f_spatial_pixels
  
  ring_number = floor(sqrt(2.)*nx/(2*step_size_pixels))
  
;; fit slope to pwr spec
  result = poly_fit(alog10(f_spatial_pixels[60:ring_number-1]),$
                    alog10(power_spec[60:ring_number-1]),1)
  
  RETURN, result[1]
END
