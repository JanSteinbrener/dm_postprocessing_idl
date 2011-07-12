;+
; NAME:
;    DM_DO_CENTER_CLICK
;
; PURPOSE:
;
;    This program finds x and y center_offset_pixels by allowing the
;    user to click the center of the adi_array.
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
;    offset_struct = dm_do_center_click(adi_array)
;
; RETURN VALUE:
;
;    offset_struct = {xcenter_offset_pixels:xcenter_offset_pixels, $
;                 ycenter_offset_pixels:ycenter_offset_pixels}
;
;
; INPUT PARAMETERS:
;
;    adi_array
;
;
; INPUT KEYWORDS:
;
;    is_data_centered: set this keyword to indicate that the incoming
;       array is alread data-centered. Otherwise it is assumed to be
;       fft-centered and will be shifted to data-centered.
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2009-02-12 JFS: added keyword is_data_centered
;-
FUNCTION dm_do_center_click, adi_array, is_data_centered=is_data_centered

  adi_size=size(adi_array)
  nx = adi_size[1]
  ny = adi_size[2]

  ;; shift to data-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,nx/2,ny/2)
  
  print, 'click on diffraction center'
  IF nx GT 1300 THEN BEGIN
     window, 0, xsize=nx/2, ysize=ny/2, title='click center'
     tvscl, alog10(congrid(adi_array+0.0000001, nx/2, ny/2))
     cursor, x, y, /dev
     x_center = x*2
     y_center = y*2
  ENDIF ELSE BEGIN
     window, 0, xsize=nx, ysize=ny, title='click center'
     tvscl, alog10(congrid(adi_array+0.0000001, nx, ny))
     cursor, x, y, /dev
     x_center = x
     y_center = y
  ENDELSE
  xcenter_offset_pixels = DOUBLE(nx/2 - x_center)
  ycenter_offset_pixels = DOUBLE(ny/2 - y_center)
  offset_struct = {xcenter_offset_pixels:xcenter_offset_pixels, $
                 ycenter_offset_pixels:ycenter_offset_pixels}

  WDELETE, 0

  ;; shift back to fft-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,nx/2,ny/2)
  
  RETURN, offset_struct
END
