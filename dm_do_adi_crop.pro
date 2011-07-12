;+
; NAME:
;    DM_DO_ADI_CROP
;
; PURPOSE:
;
;    This function crops and centers adi_array and adi_error_array 
;    using x and y center_offset_pixels from the adi_struct.  
;    If there is no adi_error_array the returned error_array is -1.
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
;    return_struct = dm_do_adi_crop(adi_struct, adi_array [,adi_error_array])
;
; RETURN VALUE:
;
;    return_struct = {adi_array:output_adi, error_array:output_error_array}
;
;
; INPUT PARAMETERS:
;
;    adi_struct
;    adi_array
;    adi_error_array:  optional
;
;
; INPUT KEYWORDS:
;
;    is_data_centered: Set this keyword to indicate that the incoming
;       arrays are already data-centered. Otherwise they are assumed
;       to be fft-centered and will be shifted accordingly.
;
;
; MODIFICATION HISTORY:
;    2009-01-15 JN: written
;    2009-02-12 JFS: added keyword is_data_centered
;-


FUNCTION dm_do_adi_crop, adi_struct, adi_array, adi_error_array, $
                         is_data_centered=is_data_centered

;; center array using stored offset values
  adi_size = size(adi_array)
  nx = adi_size[1]
  ny = adi_size[2]

  ;; shift to data centered
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions)/2))
  if N_ELEMENTS(adi_error_array) gt 1 then begin
     if not KEYWORD_SET(is_data_centered) then $
        adi_error_array = SHIFT(adi_error_array,(SIZE(adi_array,/dimensions)/2))
  endif

  x_center = nx/2 - adi_struct.xcenter_offset_pixels
  y_center = ny/2 - adi_struct.ycenter_offset_pixels

  IF adi_size[0] EQ 3 THEN BEGIN
     nz = adi_size[3]
     z_center = nz/2 - adi_struct.ycenter_offset_pixels
  ENDIF ELSE BEGIN
     nz = 1
  ENDELSE

;; crop adi_array such that diffraction pattern is centered                     
  IF (nx/2 GT x_center) THEN BEGIN
     x_index = [0,x_center*2]
  ENDIF ELSE BEGIN
     x_index = [-adi_struct.xcenter_offset_pixels*2,nx-1]
  ENDELSE

  IF (ny/2 GT y_center) THEN BEGIN
     y_index = [0,y_center*2]
  ENDIF ELSE BEGIN
     y_index = [-adi_struct.ycenter_offset_pixels*2,ny-1]
  ENDELSE

  IF nz GT 1 THEN BEGIN
     IF (nz/2 GT z_center) THEN BEGIN
        z_index = [0,z_center*2]
     ENDIF ELSE BEGIN
        z_index = [-adi_struct.zcenter_offset_pixels*2,nz-1]
     ENDELSE
  ENDIF

;;------------------------------------------
  IF nz GT 1 THEN BEGIN
;; 3D case
     crop_adi = adi_array[x_index[0]:x_index[1],y_index[0]:y_index[1], $
                         z_index[0]:z_index[1]]
     if N_ELEMENTS(adi_error_array) gt 1 then begin
        crop_adi_error = adi_error_array[x_index[0]:x_index[1], $
                                         y_index[0]:y_index[1], $
                                         z_index[0]:z_index[1]]
     endif
     
;; further crop to make square_adi                                              
     new_size=size(crop_adi)
     diff_xy = abs(new_size[1]-new_size[2])/2

     IF (new_size[1] GT new_size[2]) THEN BEGIN
        ; crop x
        square_adi = crop_adi[(diff_xy):new_size[1]-diff_xy-1,*,*]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = $
              crop_adi_error[(diff_xy):new_size[1]-diff_xy-1,*,*]
        endif
     ENDIF ELSE IF (new_size[1] LT new_size[2]) THEN BEGIN
        ; crop y
        square_adi = crop_adi[*,(diff_xy):new_size[2]-diff_xy-1,*]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = $
              crop_adi_error[*,(diff_xy):new_size[2]-diff_xy-1,*]
        endif
     ENDIF ELSE BEGIN
        square_adi = crop_adi
        if N_ELEMENTS(adi_error_array) gt 1 then begin           
           square_adi_error = crop_adi_error
        endif
     ENDELSE

     square_size = size(square_adi)
     diff_xz = abs(square_size[1]-square_size[3])/2

     IF (square_size[1] GT square_size[3]) THEN BEGIN
        ; crop x and y
        square_adi = square_adi[(diff_z):square_size[1]-diff_z-1, $ 
                                (diff_z):square_size[1]-diff_z-1,*]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = $
              square_adi_error[(diff_z):square_size[1]-diff_z-1, $
                               (diff_z):square_size[1]-diff_z-1,*]
        endif
     ENDIF ELSE IF (new_size[1] LT new_size[3]) THEN BEGIN
        ; crop z
        square_adi = square_adi[*,*,(diff_z):square_size[3]-diff_z-1]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = square_adi_error[*,*, $
                                               (diff_z):square_size[3]-diff_z-1]
        endif
     ENDIF
     square_size = size(square_adi)

;; check if adi_array size is an even number (reconstruction fails
;; with odd arrays)
     IF (square_size[1] MOD 2 NE 0) THEN BEGIN
        square_adi = square_adi[1:square_size[1]-1,1:square_size[2]-1, $
                               1:square_size[3]-1]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = square_adi_error[1:square_size[1]-1, $
                                               1:square_size[2]-1, $
                                               1:square_size[3]-1]
        endif
     ENDIF
;;------------------------------------------
  ENDIF ELSE BEGIN
;; 2D case
     crop_adi = adi_array[x_index[0]:x_index[1],y_index[0]:y_index[1]]
     if N_ELEMENTS(adi_error_array) gt 1 then begin
        crop_adi_error = adi_error_array[x_index[0]:x_index[1], $
                                         y_index[0]:y_index[1]]
     endif
;; further crop to make square_adi                                              
     new_size=size(crop_adi)
     diff = abs(new_size[1]-new_size[2])/2
     
     IF (new_size[1] GT new_size[2]) THEN BEGIN
        square_adi = crop_adi[(diff):new_size[1]-diff-1 ,*]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = crop_adi_error[(diff):new_size[1]-diff-1 ,*]
        endif
     ENDIF ELSE IF (new_size[1] LT new_size[2]) THEN BEGIN
        square_adi = crop_adi[*,(diff):new_size[2]-diff-1]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = crop_adi_error[*,(diff):new_size[2]-diff-1]
        endif
     ENDIF ELSE BEGIN
        square_adi = crop_adi
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = crop_adi_error
        endif
     ENDELSE
     
     square_size = size(square_adi)
     
;; check if adi_array is square
     IF (square_size[1] GT square_size[2]) THEN BEGIN
        square_adi = square_adi[1:square_size[1]-1, *]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = square_adi_error[1:square_size[1]-1, *]
        endif
     ENDIF
     IF (square_size[2] GT square_size[1]) THEN BEGIN
        square_adi = square_adi[*,1:square_size[2]-1]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = square_adi_error[*,1:square_size[2]-1]
        endif
     ENDIF
     
     square_size = size(square_adi)
;; check if adi_array size is an even number (reconstruction fails
;; with odd arrays)
     IF (square_size[1] MOD 2 NE 0) THEN BEGIN
        square_adi = square_adi[1:square_size[1]-1,1:square_size[2]-1]
        if N_ELEMENTS(adi_error_array) gt 1 then begin
           square_adi_error = square_adi_error[1:square_size[1]-1, $
                                               1:square_size[2]-1]
        endif
     ENDIF
  ENDELSE

  output_adi = square_adi
  if N_ELEMENTS(adi_error_array) gt 1 then begin
     output_error_array = square_adi_error
  endif else begin
     output_error_array = -1
  endelse

  ;; shift both original and cropped array back to fft centered
  if not KEYWORD_SET(is_data_centered) then begin
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions)/2))
     output_adi = SHIFT(output_adi,(SIZE(output_adi,/dimensions)/2))
  endif
  if N_ELEMENTS(adi_error_array) gt 1 then begin
     if not KEYWORD_SET(is_data_centered) then begin
        adi_error_array = SHIFT(adi_error_array,(SIZE(adi_array,/dimensions)/2))
        output_error_array = $
           SHIFT(output_error_array,(SIZE(output_adi,/dimensions)/2))
     endif
  endif

  return_struct = {adi_array:output_adi, error_array:output_error_array}

  return, return_struct
END
