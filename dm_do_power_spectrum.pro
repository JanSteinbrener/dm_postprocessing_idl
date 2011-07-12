;+
; NAME:
;    DM_DO_POWER_SPECTRUM
;
; PURPOSE:
;
;    This function calculates the average intensity of an adi_array at 
;    specified rings of spatial frequency.
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
;    power_spec_struct = dm_do_power_spectrum(adi_array
;       [,step_size_pixels = step_size_pixels, /is_amplitude])
;
; RETURN VALUE:
;
;    power_spec_struct = {power_spec:power,
;                         f_spatial_pixels:f_spatial_pixels,
;                         nobeamstop_i:nobeamstop_i}
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
;       Default is 1.0
;
;    /is_magnitude:  set if input array is magnitude (not intensity)
;
;    /is_data_centered: set this keyword to indicate that input array
;       is data-centered. Otherwise it is assumed to be fft-centered
;       and shifted accordingly.
;
; MODIFICATION HISTORY:
;    2009-01-19 JN: written
;    2009-02-12: JFS: added keyword /is_data_centered 
;    2009-05-28 JN:  removed zero magnitudes from average
;    2009-07-29 JFS: first index after beamstop will be computed and
;                    returned with return structure 
;-

FUNCTION dm_do_power_spectrum, adi_array, step_size_pixels=step_size_pixels,$
                               is_magnitude=is_magnitude, $
                               is_data_centered=is_data_centered

  ;; shift to data-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions))/2)

  IF keyword_set(is_magnitude) THEN BEGIN
     diff_intensity = adi_array^2
  ENDIF ELSE BEGIN
     diff_intensity = adi_array
  ENDELSE

  IF NOT keyword_set(step_size_pixels) THEN step_size_pixels = 1.0
  
  size_array = size(adi_array)
  nx = size_array[1]
  ny = size_array[2]

  dummy = shift(dist(nx,ny),nx/2,ny/2)
  ring_number = floor(sqrt(2.)*nx/(2*step_size_pixels))
  power = fltarr(ring_number)
  f_spatial_pixels = fltarr(ring_number)

  ;; attempt to determine extent of BS
  nobeamstop_i = 0
  check_bs = 1
  
  ;; calculate power spectrum
  FOR i = 0, (ring_number-1) DO BEGIN
     rad_in = i*step_size_pixels
     rad_out = (i+1)*step_size_pixels
     f_spatial_pixels[i] = (rad_in + rad_out)/2.

     index = where((dummy GE rad_in) AND (dummy LT rad_out) AND $
                   (diff_intensity NE 0.0),count)
     IF count ne 0 THEN BEGIN
        power[i] = total(diff_intensity[index])/float(count)
        ;; check extent of beamstop
        while check_bs eq 1 do begin
           dumb = WHERE(diff_intensity[index] eq 0, count2)
           if count2 eq 0 then begin
              nobeamstop_i = ROUND(f_spatial_pixels[i])
              check_bs = 0
           endif 
        endwhile
     ENDIF
  ENDFOR

  ;; shift back to fft-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions))/2)

  power_spec_struct = {power_spec:power, f_spatial_pixels:f_spatial_pixels, $
                       nobeamstop_i:nobeamstop_i}
  RETURN, power_spec_struct

END
