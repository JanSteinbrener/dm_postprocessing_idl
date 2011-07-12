;+
; NAME:
;    DM_GLOBAL_PHASE
;
; PURPOSE:
;
;    This program uses dm_do_global_phase to set the global phase of
;    the itn_array to be zero when its real part is maximized.  The
;    modifications are saved as a new h5 file.
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
;    dm_global_phase, filename [,reference_filename,
;                     new_name=new_name, chapman=chapman]
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
; OPTIONAL INPUT PARANETERS:
;
;    reference_filename: If it is passed along, the global phase of 
;                        itn_array will be adjusted wrt the itn_array
;                        of reference_filename.
;                        Note that this automatically invokes /chapman
;
;
; OPTIONAL INPUT KEYWORDS:
;
;    /new_name: Default name is old name appended with "global"
;    /chapman:  Set this keyword to compute the global phase 
;               according to Chapman, JOSAA 2006. 
;
;
; MODIFICATION HISTORY:
;    2009-01-16 JN: written
;    2009-06-02 JN: allowed ainfo_struct to be undefined
;    2009-09-04 JFS: added Chapman way of adjusting global phase for 
;                    one array or wrt a reference array
;-
PRO dm_global_phase, filename, reference_filename, new_name=new_name, $
                     help=help, chapman=chapman

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage: dm_global_phase, filename [,new_name = new_name]'
     print, 'Sets global phase to be zero when real part is maximized'
     print, 'and saves it as a new h5 array.'
     RETURN
  ENDIF
 
  ;; determine main file to be adjusted
  if N_ELEMENTS(reference_filename) ne 0 then $
     DM_ITN_ARRAY, reference_filename, reference_itn
  
  ;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  
  filename = STRJOIN([datafile_base,'.h5'])

;; check if we have to make our own name
  IF (NOT KEYWORD_SET(new_name)) OR (SIZE(new_name,/type) NE 7) THEN BEGIN
     ;; create the new file name by appending "global"
     pathsep = path_sep()
     new_name = Strjoin([datafile_base,'_global.h5'])
  ENDIF

;; open .h5 intensity data                                                      
  IF (dm_h5_openread(filename, h5_file_id, error_string) EQ 1) THEN BEGIN
     print, 'Error opening "'+filename+'" for reading'
     RETURN
  ENDIF
  print, 'Opened file "'+filename+'"'

  ;; create all the new structures
  dm_init_adi_struct, adi_struct
  dm_init_spt_struct, spt_struct
  dm_init_ainfo_struct, ainfo_struct
  dm_init_itn_struct, itn_struct

;; read arrays and structures from old file
  IF (dm_h5_read_adi(h5_file_id, adi_struct, adi_array, adi_error_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF  
  IF (dm_h5_read_spt(h5_file_id,spt_struct,spt_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF
  ainfo = 1
  IF (dm_h5_read_ainfo(h5_file_id, ainfo_struct,error_string) EQ 1) THEN $
     BEGIN
     ainfo = 0
     print, error_string
  ENDIF
  IF (dm_h5_read_comments(h5_file_id,specimen_name,collection_date,$
                          n_strings,comment_strings,$
                          error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF
  IF (dm_h5_read_itn(h5_file_id,itn_struct,itn_array,recon_errors, $
                     error_string) EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
     RETURN
  ENDIF

  ;; close the old file
  dm_h5_close,h5_file_id

  ;; set global phase of the main file. If the second argument is 
  ;; non-zero then it will set the global first of the first array
  ;; wrt the second array Chapman-style!
  new_itn_array = dm_do_global_phase(itn_array,reference_itn,chapman=chapman)

;; create the new file with the new name
  IF (dm_h5_create(new_name,h5_file_id,error_string) EQ 1) THEN BEGIN
     print, error_string
     RETURN
  ENDIF
  
  ;; Write arrays and structures into new file
  IF (dm_h5_write_adi(h5_file_id,adi_struct,adi_array,adi_error_array,$
                      error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF 
  IF (dm_h5_write_spt(h5_file_id,spt_struct,spt_array,$
                      error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF 
  IF ainfo EQ 1 THEN BEGIN
     IF (dm_h5_write_ainfo(h5_file_id,ainfo_struct,error_string) EQ 1) THEN $
        BEGIN
        print, error_string
     ENDIF
  ENDIF
  IF (dm_h5_write_itn(h5_file_id,itn_struct,new_itn_array, recon_errors, $
                      error_string) EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
     RETURN
  ENDIF 
  print, 'Saved to '+new_name

  ;; Don't forget to free this
  dm_ptr_free_ainfo, ainfo_struct

;; Write the comments group
  IF (dm_h5_create_comments(h5_file_id,specimen_name,collection_date,$
                            comment_strings,error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF
  ;; add comment to group
  comment_added = ['DM_GLOBAL_PHASE, '+systime()]
  IF (dm_h5_add_comments(h5_file_id,comment_added,error_string)$
      EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
     RETURN
  ENDIF ELSE BEGIN
     print, 'Added comments'
  ENDELSE

  dm_h5_close, h5_file_id

;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
 
END 
