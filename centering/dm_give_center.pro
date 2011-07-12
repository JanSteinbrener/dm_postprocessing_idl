;+
; NAME:
;    DM_GIVE_CENTER
;
; PURPOSE:
;
;    The program allows the user to change the x and y center offset
;    stored in the adi_struct by either giving the center position in
;    pixels or the offset in pixels.  First the user is asked if
;    he/she wishes to enter the center pixel.  If not, the user is
;    asked if he/she wishes to enter the value of the offset from the center.
;
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
;    Reconstruction algorithms: dm_recon/center
;
;
; CALLING SEQUENCE:
;
;    dm_give_center, filename
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
; INPUT KEYWORDS:
;
;    none
;
;
; MODIFICATION HISTORY:
;    2008-06-04 JN: Written
;    2009-01-16 JN: Changed name to dm_give_center
;
;-

PRO dm_give_center, filename, help=help

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_center_offset, filename'
     print, 'Allows the user to store a new x and y center offset by either' 
     print, 'entering the position of the center in pixels or by entering the' 
     print, 'offset from the center in pixels.'
     print, 'true center = array center - offset'
     RETURN
  ENDIF
  
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

  IF (dm_h5_read_adi(h5_file_id,adi_struct,adi_array,$
                     adi_error_array,error_string) EQ 1) THEN BEGIN
     print,error_string
  ENDIF
  
  print, 'x and y center offsets in pixels: '$
         +strtrim(STRING(FLOOR(adi_struct.xcenter_offset_pixels)),1)+' and '$
         +strtrim(STRING(FLOOR(adi_struct.ycenter_offset_pixels)),1)
  
  adi_size = size(adi_array)
  nx = adi_size[1]
  ny = adi_size[2]
  x_center = nx/2 - adi_struct.xcenter_offset_pixels
  y_center = ny/2 - adi_struct.ycenter_offset_pixels
  
  print, 'diffraction center is: '+strtrim(STRING(FLOOR(x_center)),1)+' and '$
         +strtrim(STRING(FLOOR(y_center)),1)
  
;; Change center offsets
  response = ''
  read,'Enter new OFFSETS? [y/n] ', response
  IF (response EQ 'y') THEN BEGIN
     read, 'Enter new x OFFSET in pixels: ', x_offset
     read, 'Enter new y OFFSET in pixels: ', y_offset
     read, 'Are the new offsets '+strtrim(STRING(FLOOR(x_offset)),1)+' and '$
           +strtrim(STRING(FLOOR(y_offset)),1)+'? ', response
     WHILE (response NE 'y') DO BEGIN
        read, 'Enter new x OFFSET  in pixels: ', x_offset
        read, 'Enter new y OFFSET  in pixels: ', y_offset
        read, 'Is the new offset '+strtrim(STRING(FLOOR(x_offset)),1)+' and '$
              +strtrim(STRING(FLOOR(y_offset)),1)+'? ', response
     ENDWHILE
     adi_struct.xcenter_offset_pixels = FLOAT(x_offset)
     adi_struct.ycenter_offset_pixels = FLOAT(y_offset)

  ENDIF ELSE BEGIN
;;    Change center position
     response = ''
     read,'Enter new CENTER? [y/n] ', response
     IF (response EQ 'y') THEN BEGIN
        read, 'Enter new x CENTER in pixels: ', x_center
        read, 'Enter new y CENTER  in pixels: ', y_center
        read, 'Is the new center '+strtrim(STRING(FLOOR(x_center)),1)+' and '$
              +strtrim(STRING(FLOOR(y_center)),1)+'? ', response
        WHILE (response NE 'y') DO BEGIN
           read, 'Enter new x CENTER  in pixels: ', x_center
           read, 'Enter new y CENTER  in pixels: ', y_center
           read, 'Is the new center '+strtrim(STRING(FLOOR(x_center)),1)+$
                 ' and '+strtrim(STRING(FLOOR(y_center)),1)+'? ', response
        ENDWHILE
        adi_struct.xcenter_offset_pixels = FLOAT(nx/2 - x_center)
        adi_struct.ycenter_offset_pixels = FLOAT(ny/2 - y_center)
     ENDIF
  ENDELSE
;; update changes to /adi group
  IF (dm_h5_write_adi(h5_file_id,adi_struct,adi_array,adi_error_array,$
                      error_string) EQ 1) THEN BEGIN
     print,error_string
     dm_h5_close,h5_file_id
     RETURN
  ENDIF
  print,'  Wrote "/adi" group'
  dm_h5_close,h5_file_id   

;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
  
END

