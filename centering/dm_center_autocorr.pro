;+
; NAME:
;    DM_CENTER_AUTOCORR
;
; PURPOSE:
;
;    The program calls dm_do_center_autocorr to estimate the center
;    offset in x and y of the adi_array relative to the center of the
;    recorded ccd array.  The center is determined by comparing the
;    ratio of the sum of the absolute value of the imaginary part to
;    the sum of the absolute value of the real part of the
;    autocorrelation at different centers.  The center location with
;    the lowest ratio is the best estimate for the true center of the 
;    diffraction pattern.
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
;    DM_CENTER_AUTOCORR, filename [,sigma=sigma, /click]
;
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    filename:  A string consisting of the .h5 file name    
;
;
; OPTIONAL INPUT:
;
;    none
;
;
; INPUT KEYWORDS:
;
;    sigma:  Sigma for guassian blur before autocorrelation
;       calculation.  If not set, default is used. 
;   
;    click:  Set this keyword to choose an approximate center by
;       clicking around which the center will be shifted. Otherwise,
;       the shifting is done around the center calculated by the
;       stored center offset.
;
;    is_data_centered: Set this keyword to not have
;       DM_DO_CENTER_AUTOCORR shift the adi_array to data-centered
;       before cropping and back afterwards. You should set this
;       keyword if your input array is data-centered already
;
; MODIFICATION HISTORY:
;    2008-06-04 JN: Written
;    2008-09-30 JN: modified high pass filter
;    2009-01-19 JN: renamed dm_center_autocorr and added
;                   dm_do_center_autocorr
;    2009-02-12 JS: added keyword is_data_centered
;-

PRO dm_center_autocorr, filename, sigma=sigma, click=click, help=help, $
                        is_data_centered=is_data_centered

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_autocorr_center, filename [,sigma=sigma, /click, 
     PRINT,'         /is_data_centered]'
     print, 'Finds center offset by minimizing imaginary part of' 
     print, 'autocorrelation. The offset with the lowest ratio of imaginary'
     print, 'to real part is the best estimate on the center.'
     print, 'true center = array center - offset'
     RETURN
  ENDIF
;;------------------------------------------
;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; reads hdf5 file
  IF (dm_h5_openwrite(filename,h5_file_id,error_string) EQ 1) THEN BEGIN
     print,'Error opening "'+filename+'" for writing'
     RETURN
  ENDIF
  print,'Opened file "'+filename+'"'
  
;; read adi
  IF (dm_h5_read_adi(h5_file_id,adi_struct,adi_array,$
                     adi_error_array,error_string) EQ 1) THEN BEGIN
     print,error_string
  ENDIF
  
  print, 'x and y center offsets in pixels: '$
         +strtrim(STRING(FIX(adi_struct.xcenter_offset_pixels)),1)+' and '$
         +strtrim(STRING(FIX(adi_struct.ycenter_offset_pixels)),1)
;; click on center if keyword used
  IF keyword_set(click) THEN BEGIN
     offset_struct = dm_do_center_click(adi_array, $
                                        is_data_centered = is_data_centered)
     adi_struct.xcenter_offset_pixels = offset_struct.xcenter_offset_pixels
     adi_struct.ycenter_offset_pixels = offset_struct.ycenter_offset_pixels
     
     str_xoffset = STRTRIM(STRING(FLOOR(adi_struct.xcenter_offset_pixels)),1)
     str_yoffset = STRTRIM(STRING(FLOOR(adi_struct.ycenter_offset_pixels)),1)
     print, 'New x and y center offsets are '+str_xoffset+' and '$
            +str_yoffset+' pixels.'    
  ENDIF

  offset_struct = $
     dm_do_center_autocorr(adi_struct, adi_array, min_count, $
                           sigma=sigma,is_data_centered=is_data_centered)

  adi_struct.xcenter_offset_pixels = offset_struct.xcenter_offset_pixels
  adi_struct.ycenter_offset_pixels = offset_struct.ycenter_offset_pixels

  print, 'The minimum ratio is at the x, y offset '$
         +strtrim(STRING(FIX(adi_struct.xcenter_offset_pixels)),1)+', '$
         +strtrim(STRING(FIX(adi_struct.ycenter_offset_pixels)),1)
  print, 'There are '+strtrim(STRING(min_count),1)+' minimum positions.'

  response = ''
  read,prompt='Would you like to save this as your new offset? [y/n] ',response
  IF (response EQ 'y') THEN BEGIN
     IF (dm_h5_write_adi(h5_file_id,adi_struct,adi_array,adi_error_array,$
                         error_string) EQ 1) THEN BEGIN
        print,error_string
        dm_h5_close,h5_file_id
        RETURN
     ENDIF ELSE BEGIN
        print, 'Changed the x and y center offsets.'
     ENDELSE
  ENDIF
  dm_h5_close, h5_file_id
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END
