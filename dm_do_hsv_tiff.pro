;+
; NAME:
;    DM_DO_HSV_TIFF
;
; PURPOSE:
;
;    This program creates a tif file from the given array such that
;    the phase is displayed as hue and the amplitude is the
;    saturation. This is how the PNAS 2005, cell is displayed. Use the
;    keyword to invert the display to have a white background. 
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
;    return = dm_do_hsv_tiff(array [,tiff_file = tiff_file,
;    white_bkgrd = white_bkgrd, rotate_colors=rotate_colors])
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    array
;
;
; OPTIONAL INPUT KEYWORDS:
;
;    tiff_file:  Default name is hsv.tif
;    white_bkgrd:  Creates tiff that displays the amplitude as the
;                  value.
;    rotate_colors: Set this keyword to an angle that you want the 
;                   colorwheel rotated by.
;    eps: Set this keyword to also save an eps file
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN:  written
;    2009-01-19 JFS: changed return values to be 1 for error and 0 for 
;                    non-error.
;    2009_06_24 JFS: added keyword rotate_colors
;    2010-07-23 JFS: added keyword eps
;-
FUNCTION dm_do_hsv_tiff, array, tiff_file=tiff_file, white_bkgrd=white_bkgrd, $
                         rotate_colors=rotate_colors, eps=eps

  IF n_params() LT 1 THEN BEGIN
     print, 'return = dm_do_hsv_tiff(array [,tiff_file=tiff_file,' 
     print, '  white_bkgrd=white_bkgrd])'
     RETURN, 1
  ENDIF

 ;; check if we have to make our own
  IF NOT keyword_set(tiff_file) THEN tiff_file = 'hsv.tif'

  IF (SIZE(tiff_file,/type) NE 7) THEN BEGIN
     print, 'tiff_file must be a string.'
     RETURN, 1
  ENDIF

  svec = size(array)
  nx=svec[1]
  ny=svec[2]
  phase=atan(double(imaginary(array)),double(real_part(array)))*(180./!pi)
  
  ;; rotate colors if desired
  if KEYWORD_SET(rotate_colors) then begin
     phase += rotate_colors
     inds = WHERE(phase gt 360, count)
     if count ne 0 then $
        phase[inds] -= 360
  endif
  
  amplitude=double(abs(array))/max(double(abs(array))) ;scaled into 1
  
  image_hsv=fltarr(3,nx,ny)   
  image_hsv(0,*,*)=float(phase)   ;;Hue
  IF keyword_set(white_bkgrd) THEN BEGIN
     image_hsv(1,*,*)=float(amplitude)    ;;Saturation
     image_hsv(2,*,*)=1.0                 ;;Value is 1. for all pixels
  ENDIF ELSE BEGIN
     image_hsv(1,*,*)=1.0                  ;;Saturation is 1. for all pixels
     image_hsv(2,*,*)=float(amplitude)     ;;Value 
  ENDELSE

  color_convert,image_hsv,image_rgb,/HSV_RGB

  write_tiff, tiff_file,image_rgb, red=image_rgb(0,*,*),$
              green=image_rgb(1,*,*), blue=image_rgb(2,*,*)  

  ;; also write as eps if desired
  if KEYWORD_SET(eps) then begin
     old_plot = !D.Name
     SET_PLOT,'ps'
      
     eps_file = STRJOIN([FILE_DIRNAME(tiff_file,/mark_directory), $
                        FILE_BASENAME(tiff_file,'.tif'),'.eps'])
     DEVICE, file=eps_file,/color,bits_per_pixel=8
     TV, image_rgb, true=1
     DEVICE, /close

     SET_PLOT,old_plot
  endif

  RETURN, 0

END
