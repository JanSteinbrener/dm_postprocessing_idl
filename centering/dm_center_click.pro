;+
; NAME:
;    DM_CENTER_CLICK
;
; PURPOSE:
;
;    This program uses dm_do_center_click to modify x and y
;    center_offset_pixels by allowing the user to click the center of
;    the adi_array.
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
;    dm_center_click, filename
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    filename:  File name of hdf5.
;
;
; INPUT KEYWORDS:
;
;    is_data_centered: set this keyword to indicate that the incoming
;       array is alread data-centered. Otherwise it is assumed to be
;       fft-centered and will be shifted to data-centered.
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2009-03-05 JFS: added keyword is_data_centered
;-
PRO dm_center_click, filename, help=help, $
                     is_data_centered=is_data_centered

;; help
  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_center_click, filename'
     print, 'Modifies x and y center_offset_pixels by clicking center.'
     RETURN
  ENDIF
;;------------------------------------------
;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; open .h5 intensity data                                                      
  IF (dm_h5_openwrite(filename, h5_file_id, error_string) EQ 1) THEN BEGIN
     print, 'Error opening "'+filename+'" for writing'
     RETURN
  ENDIF
  print, 'Opened file "'+filename+'"'

  IF (dm_h5_read_adi(h5_file_id, adi_struct, adi_array, adi_error_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF  

  offset_struct = dm_do_center_click(adi_array, $
                                     is_data_centered=is_data_centered)

  adi_struct.xcenter_offset_pixels = offset_struct.xcenter_offset_pixels
  adi_struct.ycenter_offset_pixels = offset_struct.ycenter_offset_pixels

  str_xoffset = STRTRIM(STRING(FLOOR(adi_struct.xcenter_offset_pixels)),1)
  str_yoffset = STRTRIM(STRING(FLOOR(adi_struct.ycenter_offset_pixels)),1)
  print, 'New x and y center offsets are '+str_xoffset+' and '$
            +str_yoffset+' pixels.'    

  response = ''
  read,prompt='Would you like to save this as your new center? [y/n] ',response
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
  
  dm_h5_close,h5_file_id
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END
