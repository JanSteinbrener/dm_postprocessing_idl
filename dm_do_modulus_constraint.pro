;+
; NAME:
;  
;  DM_DO_MODULUS_CONSTRAINT
;
;
; PURPOSE:
;
;  This program enforces the modulus constraint on a complex object in
;  inverse space. It will either replace the magnitudes with no
;  tolerance or if the users supplies an array of error values for the
;  measured intensities then it will only replace values that lie
;  outside the error interval.
;
;
; AUTHOR:
;
;  Jan Steinbrener
;  jan@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;  Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;  pi_modulus_applied = dm_do_modulus_constraint(measured_adi,
;                         current_obj [, adi_stdevs,
;                         is_magnitudes = is_magnitudes, is_real = is_real])
;
;
; INPUTS:
;
;  measured_adi:
;     An array of measured diffraction intensities. If the Keyword
;     /is_magnitudes is set then magnitudes are assumed.
;
;  current_obj:
;     The current object in inverse space as complex array. If the
;     keyword /is_real is set the object is assumed to be in real
;     space. 
;
;
; OPTIONAL INPUTS:
;
;  adi_stdevs: 
;     An array of standard deviations for the measured diffraction
;     intensities. If the array is present, the program will only
;     replace magnitudes that are outside the interval
;     [measured_value - error, measured_value + error].
;
;
; KEYWORD PARAMETERS:
;
;  /is_real:
;     This keyword indicates that current_obj is in real space.
;
;  /is_magnitudes:
;     This keyword indicates that measured_adi is an array of
;     magnitudes rather than intensities
;
;  /is_data_centered:
;     Set this keyword to indicate that the measured_adi are already
;     data centered. Otherwise it is assumed that they are
;     fft-centered and will be shifted accordingly
;
; OUTPUTS:
;
;  pi_modulus_applied:
;     The object with modulus constraint applied as complex array. If
;     the keyword /is_real is supplied then it is returned in real
;     space, otherwise in inverse space.
;
;
; OPTIONAL OUTPUTS:
;
;  none
;
;
; MODIFICATION HISTORY:
;
;  2009-01-18 JFS: written
;  2009-02-12 JFS: added keyword /is_data_centered 
;  2009-06-29 JFS: now checks for zeros in before dividing by absolute
;                  value of current object
;
;-


FUNCTION dm_do_modulus_constraint, measured_adi, current_obj, adi_stdevs, $
                                   is_real=is_real,is_magnitudes=is_magnitudes,$
                                   is_data_centered=is_data_centered

  ;; shift to data-centered
  if not KEYWORD_SET(is_data_centered) then $
     measured_adi = SHIFT(measured_adi,(SIZE(measured_adi,/dimensions))/2)
  if N_ELEMENTS(adi_stdevs) gt 1 then begin
     if not KEYWORD_SET(is_data_centered) then $
        adi_stdevs = SHIFT(adi_stdevs,(SIZE(adi_stdevs,/dimensions))/2)
  endif

  ;; transform the current object to inverse space
  if KEYWORD_SET(is_real) then begin
     pi_modulus_applied = BH_SFFT(current_obj,/preserve)
  endif else begin
     pi_modulus_applied = current_obj
  endelse

  ;; determine all indices where we have measured data and where the
  ;; current object is defined
  indices = WHERE((measured_adi and pi_modulus_applied), count)

  if count ne 0 then begin
     if n_elements(adi_stdevs) gt 0 then begin
        ;; in this case we want to project onto nearest modulus of the
        ;; interval measured - error, measured + error
        signum = measured_adi - pi_modulus_applied
        defined = WHERE(signum,count)
        if count ne 0 then $
           signum[defined] /= ABS(signum[defined])
        
        ;; now we can project
        if KEYWORD_SET(is_magnitudes) then begin
           pi_modulus_applied[indices] = $
              (pi_modulus_applied[indices]/ABS(pi_modulus_applied[indices]))* $
              (measured_adi[indices] - signum[indices]*adi_stdevs[indices])
        endif else begin
           pi_modulus_applied[indices] = $
              (pi_modulus_applied[indices]/ABS(pi_modulus_applied[indices]))* $
              (SQRT(measured_adi[indices]) - $
               signum[indices]*adi_stdevs[indices])
        endelse
     endif else begin
        ;; forget about the errors, just set it to the measured value
        if KEYWORD_SET(is_magnitudes) then begin
           pi_modulus_applied[indices] = $
              (pi_modulus_applied[indices]/ABS(pi_modulus_applied[indices]))* $
              measured_adi[indices]
        endif else begin
           pi_modulus_applied[indices] = $
              (pi_modulus_applied[indices]/ABS(pi_modulus_applied[indices]))* $
              SQRT(measured_adi[indices])
        endelse
     endelse
  endif

  ;; transform back to real space
  if KEYWORD_SET(is_real) then begin
     pi_modulus_applied = BH_SFFT(pi_modulus_applied,/inverse, $
                                  /preserve,/overwrite)
  endif


  ;; shift to fft-centered
  if not KEYWORD_SET(is_data_centered) then $
     measured_adi = SHIFT(measured_adi,(SIZE(measured_adi,/dimensions))/2)
  if N_ELEMENTS(adi_stdevs) gt 1 then begin
     if not KEYWORD_SET(is_data_centered) then $
        adi_stdevs = SHIFT(adi_stdevs,(SIZE(adi_stdevs,/dimensions))/2)
  endif

  return, pi_modulus_applied
END
