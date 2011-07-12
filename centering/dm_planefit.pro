;+
; NAME:
;    DM_PLANEFIT
;
; PURPOSE:
;
;    The program calls dm_do_planefit to performing a least square fit
;    of z = a*x + b*y + c on the itn_array to calculate the shift
;    needed to center the adi_array.  The ROI which the user defines
;    should be within the object's support. This routine was
;    rewritten from CJ's remove_plane.c
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
;    dm_planefit, filename
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
;    none
;
; MODIFICATION HISTORY:
;    2005-12-01 HM: written
;    2008-07-30 JN: - added keyword h5 to set if filename is hdf5
;                     type
;                   - modified some variable names and eliminated
;                     some user inputs
;                   - allowed user to choose uncentered ROI
;                   - renamed dm_planfit
;    2009-01-19 JN: renamed dm_planefit and added dm_do_planefit
;-

PRO dm_planefit, filename

 IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_planefit, filename'
     print, 'A linear plane fit is done on the itn_array by performing the' 
     print, 'least square fit of z = a*x + b*y + c'
     print, 'Note:(JN) Not reliable. Test on well centered budding cell' 
     print, 'gave large shifts'
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
  IF (dm_h5_openread(filename,h5_file_id,error_string) EQ 1) THEN BEGIN
     print,'Error opening "'+filename+'" for reading'
     RETURN
  ENDIF
  print,'Opened file "'+filename+'"'
  
;; read itn_array
  IF (dm_h5_read_itn(h5_file_id,itn_struct,itn_array, $
                     error_string) EQ 1) THEN BEGIN
     print,error_string
  ENDIF

  array_size = size(itn_array)
  nx = array_size[1]
  ny = array_size[2]
  
  coeffients = dm_do_planefit(itn_array)
  
  a = coeffients[0]
  b = coeffients[1]
  c = coeffients[2]
  
;estimate the center shift in x and y
  x_shift = a * nx / (2*!pi)
  y_shift = b * ny / (2*!pi)
  
  print,'least square fit to z = ax + by + c'
  print,'a = ', a
  print,'b = ', b
  print,'c = ', c
  print,'x_shift = ', x_shift
  print,'y_shift = ', y_shift
  print,'estimated center shifts:' 
  print,'x_shift = ', round(x_shift)
  print,'y_shift = ', round(y_shift)
  
  dm_h5_close, h5_file_id
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END

