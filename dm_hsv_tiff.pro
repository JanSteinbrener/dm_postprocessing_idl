;+
; NAME:
;    DM_HSV_TIFF
;
; PURPOSE:
;
;    This program uses dm_do_hsv_tiff to create a tif file from the
;    itn_array such that the phase is displayed as hue and the
;    amplitude is the saturation.  This is how the PNAS 2005, cell is 
;    displayed.  Use the keyword to invert the display to have a white
;    background. 
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
;    dm_hsv_tiff, filename [,tiff_file=tiff_file, white_bkgrd = white_bkgrd]
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
; OPTIONAL INPUT KEYWORDS:
;
;    tiff_file:    Default name is h5 name ending with .tif
;    white_bkgrd:  Creates tiff that displays the amplitude as the
;                  value.
;    eps:          Set this keyword to also write an eps file
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2009-09-04 JFS: fixed bug with filenames
;    2010-07-23 JFS: added /eps keyword to also write an eps
;-
PRO dm_hsv_tiff, filename, tiff_file=tiff_file, white_bkgrd=white_bkgrd, $
                 eps=eps, help=help

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage: dm_hsv_tiff, filename [,tiff_file=tiff_file,'
     print, 'white_bkgrd = white_bkgrd]'
     print, 'Creates a tiff file from the itn_array such that the phase is'
     print, 'displayed as hue and the amplitude is the saturation.'
     print, 'Tiff is saved as filename.tif unless tiff_file is given.'
     print, 'To have a white background use the keyword /white_bkgrd which'
     print, 'displays the amplitude as the value instead.'
     RETURN
  ENDIF

;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; check if we have to make our own name
  IF (NOT KEYWORD_SET(tiff_file)) OR (SIZE(tiff_file,/type) NE 7) THEN BEGIN
     pathsep = path_sep()
     tiff_file = Strjoin([datafile_base,'.tif'])
  ENDIF

;; open .h5 intensity data                                                      
  IF (dm_h5_openread(filename, h5_file_id, error_string) EQ 1) THEN BEGIN
     print, 'Error opening "'+filename+'" for reading'
     RETURN
  ENDIF
  print, 'Opened file "'+filename+'"'

  IF (dm_h5_read_itn(h5_file_id,itn_struct,itn_array,recon_errors, $
                     error_string) EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
     RETURN
  ENDIF

  dm_h5_close, h5_file_id

;; create hsv tif file
  IF dm_do_hsv_tiff(itn_array, tiff_file=tiff_file, $
                    white_bkgrd=white_bkgrd, eps=eps) EQ 1 THEN BEGIN
     RETURN
  ENDIF ELSE BEGIN
     print, 'Wrote "'+tiff_file+'".'
  ENDELSE
  
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir

END
