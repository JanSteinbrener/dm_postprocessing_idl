;+
; NAME:
;    DM_DO_CENTER_AUTOCORR
;
; PURPOSE:
;
;    The program estimates the center offset in x and y of the
;    adi_array relative to the center of the recorded ccd array. The
;    center is determined by comparing the ratio of the sum of the
;    absolute value of the imaginary part to the sum of the absolute
;    value of the real part of the autocorrelation at different
;    centers.  The center location with the lowest ratio is the best
;    estimate for the true center of the diffraction pattern.
;    true center = array center - offset
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
;    offset_struct = DM_DO_CENTER_AUTOCORR(adi_struct, adi_array
;       [,min_count, sigma=sigma])
;
;
; RETURN VALUE:
;
;    offset_struct = {xcenter_offset_pixels:xcenter_offset_pixels, $
;                 ycenter_offset_pixels:ycenter_offset_pixels}
;
;
; INPUT PARAMETERS:
;
;    adi_struct:  The center offset positions are changed by the
;       function
;    adi_array    
;
;
; OPTIONAL OUTPUT:
;
;    min_count:  Counts the number of positions with minimal ratios.
;
;
; INPUT KEYWORDS:
;
;    sigma:  Sigma for guassian blur before autocorrelation
;       calculation.  If not set, default is used. 
;
;    is_data_centered: set this keyword to indicate that the incoming
;       array is already data-centered and needs no shifting
;
;
; MODIFICATION HISTORY:
;    2009-01-19 JN: written
;    2009-02-12 JFS: added is_data_centered keyword.
;-

FUNCTION dm_do_center_autocorr, adi_struct, adi_array, min_count, sigma=sigma, $
                                is_data_centered=is_data_centered

  old_offset_pixels = [adi_struct.xcenter_offset_pixels, $
                       adi_struct.ycenter_offset_pixels]
  min_ratio = 100
  min_offset_pixels = old_offset_pixels
  min_count = 0

  ;; shift the array to data centered if it is not already so. NOTE
  ;; that no matter what, from here on the array will be data-centered!
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions)/2))
    
;; shift in x direction
  FOR i=-3, 3 DO BEGIN
     adi_struct.xcenter_offset_pixels = old_offset_pixels[0] + i
;; take adi_array and crop it using x and y center_offsets
     
     adi_array_struct = dm_do_adi_crop(adi_struct, adi_array, $
                                       /is_data_centered)

     adi_size = size(adi_array_struct.adi_array)
     nx = adi_size[1]
     ny = adi_size[2]

     autocorr = dm_do_autocorr(adi_array_struct.adi_array,sigma=sigma,/complex, $
                              /is_data_centered)
     ratio =  total(abs(imaginary(autocorr)))/total(abs(real_part(autocorr)))
     print, 'For an offset of '$
            +strtrim(STRING(FIX(adi_struct.xcenter_offset_pixels)),1)+' and '$
            +strtrim(STRING(FIX(adi_struct.ycenter_offset_pixels)),1)+$
            ' the ratio is '+strtrim(STRING(ratio),1)
     IF min_ratio GE ratio THEN BEGIN 
        IF min_ratio EQ ratio THEN BEGIN
           min_count = min_count+1
        ENDIF ELSE BEGIN
           min_count = 1
        ENDELSE
        min_ratio = ratio
        min_offset_pixels = $
           [adi_struct.xcenter_offset_pixels,adi_struct.ycenter_offset_pixels]
     ENDIF
  ENDFOR
;reset x offset
  adi_struct.xcenter_offset_pixels = old_offset_pixels[0]

;; shift in y direction (skipping 0,0 position)
  FOR i=-3, 3 DO BEGIN
     IF i NE 0 THEN BEGIN
        adi_struct.ycenter_offset_pixels = old_offset_pixels[1] + i
        adi_array_struct = dm_do_adi_crop(adi_struct, adi_array, $
                                          /is_data_centered)
        
        adi_size = size(adi_array_struct.adi_array)
        nx = adi_size[1]
        ny = adi_size[2]
        
        autocorr = dm_do_autocorr(adi_array_struct.adi_array, sigma=sigma, $
                                  /complex, /is_data_centered)
        ratio =  total(abs(imaginary(autocorr)))/total(abs(real_part(autocorr)))
        print, 'For an offset of '$
               +strtrim(STRING(FIX(adi_struct.xcenter_offset_pixels)),1)+ $
               ' and '+strtrim(STRING(FIX(adi_struct.ycenter_offset_pixels)),1)$
               +' the ratio is '+strtrim(STRING(ratio),1)
        IF min_ratio GE ratio THEN BEGIN 
           IF min_ratio EQ ratio THEN BEGIN
              min_count = min_count+1
           ENDIF ELSE BEGIN
              min_count = 1  
           ENDELSE
           min_ratio = ratio
           min_offset_pixels = $
              [adi_struct.xcenter_offset_pixels, $
               adi_struct.ycenter_offset_pixels] 
        ENDIF
     ENDIF
  ENDFOR
  adi_struct.ycenter_offset_pixels = old_offset_pixels[1]

;; shift the diagonals
  FOR i=-2, 2 DO BEGIN
     IF i NE 0 THEN BEGIN
        adi_struct.xcenter_offset_pixels = old_offset_pixels[0] + i
        adi_struct.ycenter_offset_pixels = old_offset_pixels[1] + i
        adi_array_struct = dm_do_adi_crop(adi_struct, adi_array,$
                                         /is_data_centered)
        
        adi_size = size(adi_array_struct.adi_array)
        nx = adi_size[1]
        ny = adi_size[2]
        
        autocorr = dm_do_autocorr(adi_array_struct.adi_array, sigma=sigma, $
                                  /complex,/is_data_centered)
        ratio =  total(abs(imaginary(autocorr)))/total(abs(real_part(autocorr)))
        print, 'For an offset of '$
               +strtrim(STRING(FIX(adi_struct.xcenter_offset_pixels)),1)+ $
               ' and '+strtrim(STRING(FIX(adi_struct.ycenter_offset_pixels)),1)$
               +' the ratio is '+strtrim(STRING(ratio),1)
        IF min_ratio GE ratio THEN BEGIN 
           IF min_ratio EQ ratio THEN BEGIN
              min_count = min_count+1
           ENDIF ELSE BEGIN
              min_count = 1 
           ENDELSE
           min_ratio = ratio
           min_offset_pixels = $
              [adi_struct.xcenter_offset_pixels, $
               adi_struct.ycenter_offset_pixels]
        ENDIF
     ENDIF
  ENDFOR
  adi_struct.xcenter_offset_pixels = old_offset_pixels[0]
  adi_struct.ycenter_offset_pixels = old_offset_pixels[1]
  FOR i=-2, 2 DO BEGIN
     IF i NE 0 THEN BEGIN
        adi_struct.xcenter_offset_pixels = old_offset_pixels[0] - i
        adi_struct.ycenter_offset_pixels = old_offset_pixels[1] + i
        adi_array_struct = dm_do_adi_crop(adi_struct, adi_array,$
                                         /is_data_centered)
        
        adi_size = size(adi_array_struct.adi_array)
        nx = adi_size[1]
        ny = adi_size[2]
        
        autocorr = dm_do_autocorr(adi_array_struct.adi_array, sigma=sigma, $
                                  /complex, /is_data_centered)
        ratio =  total(abs(imaginary(autocorr)))/total(abs(real_part(autocorr)))
        print, 'For an offset of '$
               +strtrim(FIX(STRING(adi_struct.xcenter_offset_pixels)),1)+ $
               ' and '+strtrim(FIX(STRING(adi_struct.ycenter_offset_pixels)),1)$
               +' the ratio is '+strtrim(STRING(ratio),1)
        IF min_ratio GE ratio THEN BEGIN
           IF min_ratio EQ ratio THEN BEGIN
              min_count = min_count+1
           ENDIF ELSE BEGIN
              min_count = 1
           ENDELSE  
           min_ratio = ratio
           min_offset_pixels = $
              [adi_struct.xcenter_offset_pixels, $
               adi_struct.ycenter_offset_pixels]
        ENDIF
     ENDIF
  ENDFOR
  
  xcenter_offset_pixels = min_offset_pixels[0]
  ycenter_offset_pixels = min_offset_pixels[1]
  offset_struct = {xcenter_offset_pixels:xcenter_offset_pixels, $
                   ycenter_offset_pixels:ycenter_offset_pixels}

  ;; return to fft-centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions)/2))

  RETURN, offset_struct
END
