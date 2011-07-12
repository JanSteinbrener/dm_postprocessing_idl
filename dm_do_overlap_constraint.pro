;+
; NAME:
;   
;   DM_DO_OVERLAP_CONSTRAINT
;
;
; PURPOSE:
;
;   This function will enforce the overlap constraint as described by
;   Pierre Thibault et al. in Science 2008 given a state vector, and a
;   positions vector.
;
;
; AUTHOR:
;
;    Jan Steinbrener
;    jan@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;    Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;    new_params = dm_do_overlap_constraint(state_vector,
;                    current_object, current_probe, positions_vector, 
;                    iterations)
;
;
; RETURN VALUE:
;
;    new_params:  the updated probe and the updated object or -1 if it
;                 fails  
;
;
; INPUT PARAMETERS:
;
;    state_vector: all recorded 2D views stacked on top of each other  
;
;    current_object: the current guess of the object as 2D array
;
;    current_probe: the current guess of the probe as 2D array
;
;    positions_vector: the different probe positions
;
;    max_iterations: the number of iterations to solving the coupled
;                    equations of probe and object.
;
; INPUT KEYWORDS:
;
;    /update_probe: Set this keyword to also update the probe
;
;    /overwrite: Set this keyword to overwrite the input variables
;                current_object and current_probe. This is a memory
;                saving feature.
;
;
; MODIFICATION HISTORY:
;    2009-03-11 JFS: written
;    2009-??-?? JFS: will update prove until the error drops below
;                    0.001 or up until max_iterations
;-

FUNCTION dm_do_overlap_constraint, state_vector, current_probe, obj_dims, $
                                   positions_vector, max_iterations, $
                                   use_probe_i, $
                                   update_probe=update_probe, $
                                   overwrite=overwrite
  
  
  ;; check input variables
  sv_dims = SIZE(state_vector,/dimensions)
  pos_dims = SIZE(positions_vector,/dimensions)
  probe_dims = SIZE(*(current_probe[0]),/dimensions)

  if ((pos_dims[0] ne 2) or (pos_dims[1] ne sv_dims[2])) then begin
     PRINT, STRJOIN(['[ERROR] DM_DO_OVERLAP_CONSTRAINT: position vector not ', $
                     ' in the correct format [[x1,y1],[x2,y2], ...]']) 
     PRINT, '     or number of views and number of positions are not the same!'
     return, -1
  endif
  
  if (sv_dims[0] ne probe_dims[0]) or $
     (sv_dims[1] ne probe_dims[1]) then begin
     print, STRJOIN(['[ERROR] DM_DO_OVERLAP_CONSTRAINT: [nx, ny] not the same',$
                     ' for the state vector and the probe array.'])
     return, -1
  endif

  ;; copy the input variables
  new_probe = PTRARR(N_ELEMENTS(current_probe))
  for i=0,N_ELEMENTS(current_probe) -1 do begin
     if KEYWORD_SET(overwrite) then begin
        new_probe[i] = PTR_NEW(*(current_probe[i]))
        PTR_FREE, current_probe[i]
     endif else begin
        new_probe[i] = PTR_NEW(*(current_probe[i]))
     endelse
  endfor

  ;; array to hold errors
  probe_error = 1
  i = 0
  ;if KEYWORD_SET(update_probe) then $
  ;   probe_errors = FLTARR(iterations)
  
  ;; main loop
  ;for i=0,iterations-1 do begin
  while ((probe_error gt 0.001) and (i lt max_iterations)) do begin
     temp_object = FLTARR(obj_dims)  
     
     ;; we need another copy to calculate the numerator
     new_object = COMPLEXARR(obj_dims)
     
     ;; update the object - denominator
     for j=0,pos_dims[1]-1 do begin
        ix_low = positions_vector[0,j]-probe_dims[0]/2
        ix_high = positions_vector[0,j]+probe_dims[0]/2-1
        iy_low = positions_vector[1,j]-probe_dims[1]/2
        iy_high = positions_vector[1,j]+probe_dims[1]/2-1
        temp_object[ix_low:ix_high,iy_low:iy_high] += $
           FLOAT(ABS(*(new_probe[use_probe_i[j]]))^2)
     endfor
     defined = WHERE(temp_object, count)
     ;js_display_image,abs(new_object)
     if count ne 0 then $
        temp_object[defined] = 1/temp_object[defined]
    
     ;; update the object - numerator
     for j=0,pos_dims[1]-1 do begin
        ix_low = positions_vector[0,j]-probe_dims[0]/2
        ix_high = positions_vector[0,j]+probe_dims[0]/2-1
        iy_low = positions_vector[1,j]-probe_dims[1]/2
        iy_high = positions_vector[1,j]+probe_dims[1]/2-1

        new_object[ix_low:ix_high,iy_low:iy_high] += $
           CONJ(*(new_probe[use_probe_i[j]]))*state_vector[*,*,j]
     endfor
     ;js_display_image,abs(temp_object)
     ;; finally combine numerator with denominator
     new_object *= temp_object

     ;js_display_image,abs(new_object),title=STRTRIM(i,2)
     drop_inds = WHERE(ABS(new_object) ge 1, count)
     if count ne 0 then $
        new_object[drop_inds] /= (ABS(new_object[drop_inds])*1.01)
     ;print,max(abs(new_object))
     ;; update probe if desired
     if KEYWORD_SET(update_probe) then begin
        
        old_probe = PTRARR(N_ELEMENTS(new_probe))
        for j=0,N_ELEMENTS(new_probe)-1 do begin
           old_probe[j] = PTR_NEW(*(new_probe[j]))
           *(new_probe[j]) *= COMPLEX(0,0)
        endfor
        
        ;; this is for the numerator
        temp_probe = FLTARR([probe_dims,N_ELEMENTS(new_probe)])

        ;; loop trough positions - denominator
        for j=0, pos_dims[1]-1 do begin
           ix_low = positions_vector[0,j]-probe_dims[0]/2
           ix_high = positions_vector[0,j]+probe_dims[0]/2-1
           iy_low = positions_vector[1,j]-probe_dims[1]/2
           iy_high = positions_vector[1,j]+probe_dims[1]/2-1
           temp_probe[*,*,use_probe_i[j]] += $
              ABS(new_object[ix_low:ix_high,iy_low:iy_high])^2
        endfor
        defined = WHERE(temp_probe, count)

        if count ne 0 then $
           temp_probe[defined] = 1/temp_probe[defined]

        ;; update the probe - numerator
        for j=0,pos_dims[1]-1 do begin
           ix_low = positions_vector[0,j]-probe_dims[0]/2
           ix_high = positions_vector[0,j]+probe_dims[0]/2-1
           iy_low = positions_vector[1,j]-probe_dims[1]/2
           iy_high = positions_vector[1,j]+probe_dims[1]/2-1
           
           *(new_probe[use_probe_i[j]]) += $
              CONJ(new_object[ix_low:ix_high,iy_low:iy_high])* $
              state_vector[*,*,j]
        endfor        
        
        ;;finally combine numerator with denominator
        for j=0, N_ELEMENTS(new_probe)-1 do begin
           *(new_probe[j]) *= temp_probe[*,*,j]
        endfor

        ;; calculate error
        probe_error = DOUBLE(0)
        for j=0,N_ELEMENTS(new_probe)-1 do begin
           probe_error += TOTAL(ABS(*(new_probe[j]) - *(old_probe[j]))^2)
        endfor
        probe_error = SQRT(probe_error)
        PRINT, 'Probe error: '+STRTRIM(probe_error,2)

        ;; free old_probe
        for j=0,N_ELEMENTS(new_probe)-1 do $
           PTR_FREE, old_probe[j]

     endif else begin
        probe_error = 0.0
     endelse
     i++
  endwhile
  
  new_params = $
     {new_object:new_object, new_probe:new_probe}
  return, new_params
END
