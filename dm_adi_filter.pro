;+
; NAME:
;    DM_ADI_FILTER
;
; PURPOSE:
;
;    This program takes the zeros in the adi_array, applies a
;    smoothing function, then applies this filter FFT of itn_array
;    using the function dm_do_adi_filter and saves the result as a new
;    h5 file. 
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
;    dm_adi_filter, filename [,new_name = new_name, 
;                             width = width, depth = depth,
;                             is_data_centered = is_data_centered]
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
;    new_name:  Default name is old name appended with "filtered"
;    width:  Smoothing width.  Default used if not given
;    depth:  Default used if not given
;    is_data_centered:  Use if adi_array is data centered
;
;
; MODIFICATION HISTORY:
;    2009-08-17 JN: written
;-
PRO dm_adi_filter, filename, new_name=new_name, width = width, depth=depth, $
                   is_data_centered=is_data_centered, help=help
  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage: dm_adi_filter, filename [,new_name = new_name,' 
     print, 'width = width, depth = depth, /is_data_centered]'
     print, 'Performs a filter on the FFT of itn_array by smoothing the' 
     print, 'unknown regions of the adi_array.'
     print, 'Default saves modified h5 as "old_name_filtered.h5"'
     print, 'Default of width and depth come from dm_do_adi_filter'
     RETURN
  ENDIF
  
;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; check if we have to make our own name
  IF (NOT KEYWORD_SET(new_name)) OR (SIZE(new_name,/type) NE 7) THEN BEGIN
     ;; create the new filename by appending "filtered"
     pathsep = path_sep()
     new_name = Strjoin([datafile_path,pathsep,datafile_base,'_filtered.h5'])
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
     dm_h5_close, h5_file_id
     RETURN
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
  IF (dm_h5_read_itn(h5_file_id,itn_struct,itn_array,recon_errors, error_string)$
      EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
     RETURN
  ENDIF

  ;; close the old file
  dm_h5_close,h5_file_id

;; do filtering 
  new_itn_array = dm_do_adi_filter(adi_array, itn_array, width, depth, $
                                   is_data_centered=is_data_centered)

;; create the new file with the new name
  IF (dm_h5_create(new_name,h5_file_id,error_string) EQ 1) THEN BEGIN
     print, error_string
     dm_h5_close, h5_file_id
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
  str_width = STRTRIM(STRING(width),1)
  str_depth = STRTRIM(STRING(depth),1)
  comment_added = ['DM_ADI_FILTER, '+systime()+': width = '+str_width+$
                   ', depth = '+str_depth]
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
