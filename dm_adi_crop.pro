;+
; NAME:
;    DM_ADI_CROP
;
; PURPOSE:
;
;    This program called the function dm_do_adi_crop which crops and 
;    centers adi_array and adi_error_array using x and y 
;    center_offset_pixels from the adi_struct.  If there is no
;    adi_error_array the returned cropped_adi_error is empty.
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
;    dm_adi_crop, filename, cropped_adi [,cropped_adi_error]
;
; RETURN VALUE:
;
;    cropped_adi:  Centered, even numbered adi_array
;    cropped_adi_error:  Cropped identically to cropped adi_array
;
;
; INPUT PARAMETERS:
;
;    filename:  File name of hdf5.
;
;
; INPUT KEYWORDS:
;
;    is_data_centered: Set this keyword to indicate that the array in
;       filename is already data-centered. Otherwise it is assumed to
;       be fft-centered and will be shifted by DM_DO_ADI_CROP.
;
;
; MODIFICATION HISTORY:
;    2009-01-15 JN: written
;    2009-02-12 JFS: added keyword /is_data_centered
;    2009-07-09 JFS: fixed bug with error_array
;    2010-04-08 JFS: another bug with the error array
;-

PRO dm_adi_crop, filename, cropped_adi, cropped_adi_error, help=help, $
                 is_data_centered=is_data_centered

;; help
  IF (keyword_set(help) OR (n_params() LT 2)) THEN BEGIN
     print, 'Usage:  dm_adi_crop, filename, cropped_adi [,cropped_adi_error,'
     PRINT, '        /is_data_centered]'
     print, 'Crops and centers adi_array and adi_error_array using saved' 
     print, 'x and y center_offset_pixels in adi_struct.'
     print, 'If there is no adi_error_array the cropped_adi_error is empty.'
     RETURN
  ENDIF
;;------------------------------------------

;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; open .h5                                                   
  IF (dm_h5_openread(filename, h5_file_id, error_string) EQ 1) THEN BEGIN
     print, 'Error opening "'+filename+'" for reading'
     RETURN
  ENDIF
  print, 'Opened file "'+filename+'.h5"'
                                                
  IF (dm_h5_read_adi(h5_file_id, adi_struct, adi_array, adi_error_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF ELSE BEGIN
     print, 'Read adi_array'
  ENDELSE

  adi_cropped_struct = dm_do_adi_crop(adi_struct, adi_array, adi_error_array, $
                                     is_data_centered=is_data_centered)

  cropped_adi = adi_cropped_struct.adi_array

  IF adi_cropped_struct.error_array[0] GT -1 THEN BEGIN
     cropped_adi_error = adi_cropped_struct.error_array
  ENDIF

  dm_h5_close, h5_file_id

;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END 
