;+
; NAME:
;
;   DM_DO_SUPPORT_CONSTRAINT
;
;
; PURPOSE:
;
;   This program enforces the support constraint for a given input
;   array with input support. It optionally will also enforce a
;   positivity constraint for both real and imaginary part or only for
;   the imaginary part.
;
;
; AUTHOR:
;   
;   Jan Steinbrener
;   jan@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;  Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;  pi_support_supplied = dm_do_support_constraint(current_obj,
;                          current_spt [, reality = reality, positivity =
;                          positivity, imaginary_only = imaginary_only])
;
;
; RETURN VALUE:
;
;  pi_support_applied: 
;     array where with applied support constraint and optionally
;     applied positivity constraint
;
;
; INPUTS:
;
;  current_obj: 
;     a complex array of the current object in real space
;
;  current_spt: 
;     a byte array of the current support
;
;
; OPTIONAL INPUTS:
;
;  none  
;
;
; KEYWORD PARAMETERS:
;
;  /reality: 
;     if this keyword is set the imaginary part of the output array
;     will be set to 0.
;
;  /positivity: 
;     also enforce positivity on both real AND imaginary
;     parts of current_obj. The default is no positivity
;     constraint. 
;
;  /imaginary_only: 
;     enforce positivity but only on the imaginary part. This keyword
;     includes the /positivity keyword. The default is no positivity
;     constraint.
; 
;
; OUTPUTS:
;
;  pi_support_applied: 
;     the complex object in real space with applied support constraint
;     and optionally applied positivity constraint.
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
;-


FUNCTION dm_do_support_constraint, current_obj, current_spt, reality=reality, $
                                   positivity=positivity, $
                                   imaginary_only=imaginary_only
  
  

  ;; real and imaginary
  real_part = REAL_PART(current_obj)
  imaginary = IMAGINARY(current_obj)

  ;; apply support constraint.
  indices = WHERE(current_spt ne 1., count)
  if (count ne 0) then begin
     real_part[indices] = 0.
     
     ;; reality?
     if KEYWORD_SET(reality) then begin
        imaginary *= 0.
     endif else begin
        imaginary[indices] = 0.
     endelse

     ;; positivity?
     if (KEYWORD_SET(positivity) or KEYWORD_SET(imaginary_only)) then begin
        indices = WHERE(imaginary lt 0.,count)
        if (count ne 0) then $
           imaginary[indices] = 0.0
        if not KEYWORD_SET(imaginary_only) then begin
           indices = WHERE(real_part lt 0.,count)
           if (count ne 0) then $
              real_part[indices] = 0.0
        endif
     endif
  endif
  
  if (SIZE(real_part,/type) eq 4) then begin
     pi_support_applied = COMPLEX(real_part, imaginary)
  endif else begin
     pi_support_applied = DCOMPLEX(real_part, imaginary)
  endelse

  return, pi_support_applied
END
