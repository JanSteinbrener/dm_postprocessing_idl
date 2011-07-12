;+
; NAME:
;    DM_DO_PRTF
;
; PURPOSE:
;
;    This function takes the itn_array and adi_array and calculates
;    the reconstructed power over known power.
;
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
;    prtf_struct = dm_do_PRTF(adi_array, average_itn)
;
; RETURN VALUE:
;
;    prtf_struct = {prtf_array,f_spatial_pixels,bs_radius_pixels}
;        
;        prtf_struct.prtf_array: A 1D array of the calculate PRTF
;
;        prtf_struct.f_spatial_pixels: A 1D array of corresponding 
;            spatial frequencies in pixels
;
;        prtf_struct.bs_radius_pixels: The last radius at which the 
;            known magnitudes where 0. For step_size > 1 the end of
;            the interval is assumed.
;
;
; INPUT PARAMETERS:
;
;    adi_array:  Array must be cropped and centered to match itn_array
;    average_itn:  An array of averaged itn_arrays.
;
;
; INPUT KEYWORDS:
;
;    /is_magnitude:  set if adi_array is a magnitude
;
;    /is_real:  set if itn_array is in real space
;
;    /step_size_pixels: set this keyword to the number of radial pixels 
;        you want the algorithm to average over. The default is 1.
;
;    /is_data_centered: set this keyword if the incoming adi_array is
;        already data-centered. Otherwise it is assumed to be
;        fft-centered and will be shifted accordingly
;
; MODIFICATION HISTORY:
;    2009-01-15 JN: written
;    2009-01-22 JN: added keyword /is_magnitude, /is_real
;    2009-01-22 JFS: added step_size_pixels keyword and calculation
;        of radius in pixels where beamstop ends. The return value is
;        now a structure with the prtf array, the spatial frequencies
;        in pixels and the beamstop cutoff radius in pixels.
;    2009-02-12 JFS: added keyword /is_data_centered
;    2009-05-28 JN:  removed zero magnitudes from average
;                    commented out bs_check
;    2009-11-25 JFS: 3Dfied. Will compute PRTF for [*,*,nz/2] slice. 
;    2009-12-15 JFS: bug fixed if there are no 0 pixels in the known
;                    magnitudes 
;-


FUNCTION dm_do_PRTF, adi_array, average_itn, is_magnitude=is_magnitude, $
                     is_real=is_real, step_size_pixels = step_size_pixels, $
                     is_data_centered=is_data_centered
  
  array_size = SIZE(average_itn,/dimensions)
  nx = array_size[0]
  ny = array_size[1]
  if N_ELEMENTS(array_size) eq 3 then $
     iz = array_size[2]/2 else iz = 0

  ;; shift to data-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,array_size/2)

  IF NOT keyword_set(step_size_pixels) THEN step_size_pixels = 1.0

  radius = shift(dist(nx,ny), nx/2, ny/2)
  radius = round(radius)
  max_rad = floor(sqrt(2)*nx/2)
  ring_number = FLOOR(max_rad/step_size_pixels)
  recon_power =  fltarr(ring_number)
  known_power =  fltarr(ring_number)
  f_spatial_pixels = fltarr(ring_number)

  IF keyword_set(is_real) THEN BEGIN
     fft_itn = (bh_sfft(average_itn, /preserve_power, /double))[*,*,iz]
  ENDIF ELSE BEGIN
     fft_itn = average_itn[*,*,iz]
  ENDELSE
  IF NOT keyword_set(is_magnitude) THEN BEGIN
     known_mag = sqrt(abs(adi_array[*,*,iz]))
  ENDIF ELSE BEGIN
     known_mag = abs(adi_array[*,*,iz])
  ENDELSE

  ;bs_check = 1
  ;bs_radius_pixels = 0
  FOR i = 0, (ring_number-1) DO BEGIN
     rad_in = i*step_size_pixels
     rad_out = (i+1)*step_size_pixels
     index_itn = where((radius GE rad_in) AND (radius LT rad_out), count_itn)
     index_mag = where((radius GE rad_in) AND (radius LT rad_out) AND $
                   (known_mag NE 0.0),count_mag)
 
     IF count_mag NE 0 THEN BEGIN
        recon_power[i] = total(abs(fft_itn[index_itn]))/float(count_itn)
        known_power[i] = total(known_mag[index_mag])/float(count_mag)
        
        ;; check extent of beamstop
        ;if (bs_check) then begin
         ;  bs_indices = WHERE(known_mag[index] eq 0,bs_count)
          ; if (bs_count ne 0) then begin
              ;; we want to be on the safe side so take upper boundary
              ;; of interval.
           ;   bs_radius_pixels = CEIL((rad_in + rad_out)/2.)
           ;endif else begin
              ;; stop at the first radius where there are no more 0s
              ;; anymore
           ;   bs_check = 0
           ;endelse
        ;endif
     ENDIF
     f_spatial_pixels[i] = (rad_in + rad_out)/2.
  ENDFOR

  ;; prevent a division by zero
  index = where(known_power EQ 0, count)
  if count ne 0 then $
     known_power[index] = 0.0000001
  
  prtf_array = recon_power/known_power

  prtf_struct = {prtf_array:prtf_array,f_spatial_pixels:f_spatial_pixels}
  ;,bs_radius_pixels:bs_radius_pixels}


  ;; shift back to fft-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,array_size/2)

  RETURN, prtf_struct

END
