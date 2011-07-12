;+
; NAME:
;    DM_DO_GLOBAL_PHASE
;
; PURPOSE:
;
;    This function sets the global phase of the itn_array to be zero
;    when its real part is maximized.  
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
;    new_itn_array = dm_global_phase(itn_array[,reference_itn,chapman=chapman])
;
; RETURN VALUE:
;
;    new_itn_array:  array with global phase set
;
;
; INPUT PARAMETERS:
;
;    itn_array
;
;
; OPTIONAL INPUT PARANETERS:
;
;    reference_itn: If it is passed along, the global phase of 
;                   itn_array will be adjusted wrt reference_itn.
;                   Note that this automatically invokes /chapman
;
;
; INPUT KEYWORDS:
;
;    /chapman:  Set this keyword to compute the global phase 
;               according to Chapman, JOSAA 2006. 
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2009-09-04 JFS: added Chapman way of adjusting global phase for 
;                    one array or wrt a reference array
;-
FUNCTION dm_do_global_phase, itn_array, reference_itn, $
                             chapman=chapman

  ;; get current phase of itn_array
  itn_phase = atan(itn_array, /phase) 

  if (KEYWORD_SET(chapman) or (N_ELEMENTS(reference_itn) ne 0)) then begin
     if N_ELEMENTS(reference_itn) eq 0 then begin
        complex_sq_sum = TOTAL(itn_array^2)
        global_phase = ATAN(complex_sq_sum,/phase)
        global_phase *= -0.5
     endif else begin
        complex_sq_sum = TOTAL(CONJ(reference_itn)*itn_array)
        global_phase = ATAN(complex_sq_sum,/phase)
     endelse
  endif else begin
     ;; find phase where real part of image is max.
     max_real = max(real_part(itn_array), max_subscript)
     global_phase = itn_phase(max_subscript)
  endelse
  
  ;; set global phase
  shifted_phase = itn_phase - global_phase
  
  gt_pi_index = where(shifted_phase GT !PI, count)
  IF count GT 0 THEN BEGIN
     shifted_phase[gt_pi_index] = shifted_phase[gt_pi_index] - 2*!PI
  ENDIF
  le_neg_pi_index = where(shifted_phase LE (-!PI), count)
  IF count GT 0 THEN BEGIN
     shifted_phase[le_neg_pi_index] = shifted_phase[le_neg_pi_index] + 2*!PI
  ENDIF
  PRINT,'DM_DO_GLOBAL_PHASE: Adjusting phase by '+strtrim(global_phase,2)+ $
        ' radians.'
  
  new_itn_array = abs(itn_array) * COMPLEX(cos(shifted_phase), $
                                           sin(shifted_phase), /double)
  RETURN, new_itn_array
END
