;+
; NAME:
;    DM_DO_PHASE_RAMP
;
; PURPOSE:
;
;    This function subtracts a phase ramp in both x and y direction
;    from the itn_array.  This file is modified from rm_phase_ramp.pro
;    from XH.
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
;    data_new = dm_do_phase_ramp(data)
;
; RETURN VALUE:
;
;    data_new:  array with linear phase ramp removed
;
;
; INPUT PARAMETERS:
;
;    data:  2D array typically an itn_array
;
;
; INPUT KEYWORDS:
;
;    none
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;-
FUNCTION dm_do_phase_ramp, data

  svec = size(data)
  nx = svec[1]
  ny = svec[2]
  
  ;; recentering the array to its mass center: (usually already centered)
  s = fltarr(nx,ny)
  index = where(abs(data) ne 0., count)
  s[index] = 1.
  x = findgen(nx)
  y = findgen(ny)
  s_mass_x = 0.
  s_mass_y = 0.
  FOR i = 0, nx - 1 DO BEGIN
     s_mass_x += total(x * s[*,i])
     s_mass_y += total(y * s[i,*])
  ENDFOR
  s_ctr_x = s_mass_x / total(s)
  s_ctr_y = s_mass_y / total(s)
  data = shift(data,floor(nx/2-s_ctr_x),floor(ny/2-s_ctr_y))
  
  phase = atan(data,/phase)
  line = fltarr(ny)

;; calculates slope in phase line-by-line
  slope = 0
  num = 0
  FOR i=0, nx-1 do begin
     index_count = where(phase[i,*] ne 0., count)
     if(count GE 10.) then begin
        line[*] = phase[i,*]
        index = where(line ne 0.)
        linear_fit = poly_fit(index, line[index],1)
        slope = slope + linear_fit[1]
        num ++
        endif
  ENDFOR
  slope = slope / num
  FOR i=0, nx-1 do begin
     if(total(phase[i,*]) ne 0.) then begin
        line[*] = phase[i,*]
        index = where(line ne 0.)
        phase[i,index] = line[index] - slope * index
     endif
  ENDFOR
  data = abs(data) * complex(cos(phase),sin(phase))

  slope = 0
  num = 0
  FOR i=0, ny-1 do begin
     index_count = where(phase[*,i] ne 0., count)
     if(count GE 10.) then begin
        line[*] = phase[*,i]
        index = where(line ne 0.)
        linear_fit = poly_fit(index, line[index],1)
        slope = slope + linear_fit[1]
        num ++
     endif
  ENDFOR
  slope = slope / num
  FOR i=0, ny-1 do begin
     if(total(phase[*,i]) ne 0.) then begin
        line[*] = phase[*,i]
        index = where(line ne 0.)
        phase[index,i] = line[index] - slope * index
     endif
  ENDFOR
  
  phase[*,*] = phase[*,*] - mean(phase)
  data_new = abs(data) * complex(cos(phase),sin(phase))

  RETURN, data_new
END
