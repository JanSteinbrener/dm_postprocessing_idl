;+
; NAME:
;
;  DM_AVERAGE_H5
;
;
; PURPOSE:
;
;  This procedure will average all .h5 files within a directory that
;  match a user supplied filter pattern (Look at IDLs documentation of
;  the function FILE_SEARCH to find a list of supported wildcards and
;  expansions). The routine will optionally attempt to remove phase
;  ramps, set the global phase and high-pass filter before averaging.
;
;
; AUTHOR:
;
;  Jan Steinbrener
;  jan@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;  Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;  dm_average_h5 [, target_dir, filter, avg_filename, hp_width,
;                  hp_depth, high_pass = high_pass,
;                  rm_phase_ramp = rm_phase_ramp, align = align,
;                  set_global_phase = set_global_phase,
;                  save_tiff = save_tiff, white_bkgrd = white_bkgrd]
; 
;
; INPUTS:
;
;  none
;
;
; OPTIONAL INPUTS:
;
;  target_dir:
;     the directory where to look for .h5 files matching the
;     filter. If not provided the current working directory will be
;     searched.
;
;  filter: 
;     the filter pattern that the .h5 files have to satisfy (Look at
;     IDLs documentation of the function FILE_SEARCH to find a list of
;     supported wildcards and expansions). If not provided, the
;     routine will average all .h5 files it can find in target_dir.
;
;  avg_filename:
;     the filename of the newly created averaged .h5 file. If not
;     provided then the routine will append _avg to the first filename
;     of the list of filenames that matched the filter criterion.
;
;  hp_width:
;     An optional width for the optional high-pass filter. This is only 
;     valid in conjunction with the /high_pass keyword. If not
;     specified the defaults of the high-pass filter function
;     DM_DO_HIGH_PASS_FILTER will be used. 
;
;  hp_depth:
;     An optional depth for the optional high-pass filter. This is only 
;     valid in conjunction with the /high_pass keyword. If not
;     specified the defaults of the high-pass filter function
;     DM_DO_HIGH_PASS_FILTER will be used. 
;
;
; KEYWORD PARAMETERS:
;
;  /rm_phase_ramp:
;     Set this keyword to remove phase ramps before averaging and
;     optional high-pass filtering and setting of the global
;     phase. The function is DM_DO_PHASE_RAMP.
;
;  /high_pass: 
;     Set this keyword to high-pass filter before averaging and
;     optional setting of the global phase. The function is
;     DM_DO_HIGH_PASS_FILTER.
;
;  /set_global_phase:
;     Set this keyword to set the global phase before averaging using
;     the DM_DO_GLOBAL_PHASE function.
;
;  /align: 
;     Set this keyword to align the arrays with the first before
;     averaging using the function DM_DO_CROSS_CORRELATION
;
;  /save_tiff:
;     Set this keyword to have the averaged result be written to disk
;     as a tiff file using DM_DO_HSV_TIFF. You can set the keyword to
;     a desired filename for the tiff, otherwise it will use the
;     avg_filename.
;
;  /white_bkgrd:
;     Set this keyword to create a tiff output on white background
;     rather than black background which is the default. If /save_tiff
;     is not invoked, this keyword is meaningless.
;
;
; OUTPUTS:
;
;  A new .h5 file in target_dir with avg_filename
;
;
; OPTIONAL OUTPUTS:
;
;  none
;
;
; MODIFICATION HISTORY:
;
;  2009-01-18 JFS: written
;  2009-01-21 JFS: added white_bkgrd keyword. Ainfo group now copied 
;     to averaged file if it exists.
;  2009-06-12 JFS: fixed bug with copying ainfo structure
;  2009-06-16 JFS: fixed bug when handing avg_filename
;  2009-10-02 JN:  set_global_phase uses keyword /chapman
;  2010-01-20 JN:  added /align
;-


PRO dm_average_h5, target_dir, filter, avg_filename, hp_width, hp_depth, $
                   high_pass=high_pass, rm_phase_ramp=rm_phase_ramp, $
                   set_global_phase=set_global_phase, align = align, $
                   save_tiff=save_tiff, white_bkgrd=white_bkgrd
  
  
  ;; help
  if (N_PARAMS() eq 0) then begin
     PRINT,'DM_AVERAGE_H5: No parameters given. Do you want to use defaults?'
     PRINT,'      Defaults: average all .h5 files in current directory.'
     answer = ''
     while ((STRCMP(answer,'y') ne 1) and $
            (STRCMP(answer,'n') ne 1)) do begin
        PRINT,'Please answer (y/n).'
        answer = GET_KBRD()
     endwhile
     if (STRCMP(answer,'n') eq 1) then begin
        PRINT,'dm_average_h5 [, target_dir, filter, avg_filename, hp_width, '
        PRINT,'                 hp_depth, high_pass=high_pass,'
        PRINT,'                 rm_phase_ramp=rm_phase_ramp,'
        PRINT,'                 set_global_phase=set_global_phase, '
        PRINT,'                 save_tiff=save_tiff, white_bkgrd=white_bkgrd]'
        return
     endif
  endif
  
  path_sep = PATH_SEP()
  
  ;; accept all .h5 files if there is no filter supplied
  if N_ELEMENTS(filter) eq 0 then $
     filter = '*.h5'
  
  ;; see if user supplied directory. If not use current dir
  if N_ELEMENTS(target_dir) eq 0 then begin
     cd, current=target_dir
  endif else begin
     ;; Take out any trailing path separator to be consistent
     if (STRPOS(target_dir,path_sep,/Reverse_Search) eq $
         (STRLEN(target_dir)-1)) then begin
        target_dir = BYTE(target_dir)
        target_dir = STRING(target_dir[0:N_ELEMENTS(target_dir)-2])
     endif
  endelse
  
  ;; create array of .h5 filenames within directory
  filenames = FILE_SEARCH(STRJOIN([target_dir,path_sep,filter]), $
                          count=count)
  
  PRINT, 'List of files to be averaged:' 
  for i=0,N_ELEMENTS(filenames)-1 do begin
     PRINT,'   '+filenames[i]
  endfor

  if count ne 0 then begin
     ;; create output filename if user did not supply one
     if N_ELEMENTS(avg_filename) eq 0 then begin
        new_dir = target_dir+path_sep+'avg'
        FILE_MKDIR,new_dir
        avg_filename = STRJOIN([new_dir,path_sep, $
                                FILE_BASENAME(filenames[0],'.h5'), $
                                '_avg.h5'])
     endif else begin
        new_dir = FILE_DIRNAME(avg_filename)
     endelse

     ;; check for save_tiff
     if KEYWORD_SET(save_tiff) then begin
        if (SIZE(save_tiff,/type) ne 7) then begin
           save_tiff = STRJOIN([new_dir,path_sep, $
                                FILE_BASENAME(avg_filename,'.h5'),'.tif'])
        endif
     endif

     ;; open first file
     if (DM_H5_OPENREAD(filenames[0],h5_id,error_string) eq 1) then begin
        print, error_string
        return
     endif
     
     ;; get the adi_structure
     if (DM_H5_READ_ADI(h5_id,adi_struct,adi_array, $
                        adi_error_array,error_string) eq 1) then begin
        print, error_string
        ;return
     endif
     
     ;; get ainfo structure
     if (DM_H5_READ_AINFO(h5_id, ainfo_struct,error_string) eq 1) then $
        begin
        PRINT,error_string
        ainfo_exists = 0
     endif else begin
        ainfo_exists = 1
     endelse

     ;; get comments
     if (DM_H5_READ_COMMENTS(h5_id,specimen_name,collection_date,$
                             n_strings,comments,$
                             error_string) eq 1) then begin
        print,error_string
        ;dm_h5_close,h5_id
        ;return
     endif
     
     ;; get current iterate
     if (DM_H5_READ_ITN(h5_id,itn_struct,avg_obj,recon_errors,$
                        error_string) eq 1) then begin
        print,error_string
        dm_h5_close,h5_id
        return
     endif
     
     ;; remove phase ramps
     if KEYWORD_SET(rm_phase_ramp) then $
        avg_obj = DM_DO_PHASE_RAMP(avg_obj)

     ;; set global phase
     if KEYWORD_SET(set_global_phase) then $
        avg_obj = DM_DO_GLOBAL_PHASE(avg_obj, /chapman)

     ;; hp filter
     if KEYWORD_SET(high_pass) then $
        avg_obj = DM_DO_HIGH_PASS_FILTER(avg_obj,hp_width,hp_depth)
     
     ;; close file
     DM_H5_CLOSE, h5_id
     
     ;; cycle through all files and average
     for i=1,count-1 do begin
        DM_ITN_ARRAY,filenames[i],current_obj

        ;; remove phase ramps
        if KEYWORD_SET(rm_phase_ramp) then $
           curent_obj = DM_DO_PHASE_RAMP(current_obj)        

        ;; set global phase
        if KEYWORD_SET(set_global_phase) then $
           current_obj = DM_DO_GLOBAL_PHASE(current_obj, avg_obj)

        ;; hp filter
        if KEYWORD_SET(high_pass) then $
           current_obj = DM_DO_HIGH_PASS_FILTER(current_obj,hp_width,hp_depth)

        ;; align
        if KEYWORD_SET(align) then begin
           ;convolution, avg_obj, current_obj, shifted_obj
           cross_correlation = DM_DO_CROSS_CORRELATION(current_obj, avg_obj, $
                                                       shift_value)
           current_obj = SHIFT(current_obj, shift_value)
           print, shift_value
        endif
        
        avg_obj += current_obj
     endfor

     ;; normalize
     avg_obj /= count
     
     ;; create file
     if (DM_H5_CREATE(avg_filename,h5_id,error_string) eq 1) then begin
        PRINT,error_string
        return
     endif

     ;; write adi_struct
     if (DM_H5_WRITE_ADI(h5_id,adi_struct,adi_array, $
                         adi_error_array,error_string) eq 1) then begin
        PRINT, error_string
        ;DM_H5_CLOSE, h5_id
        ;return
     endif

     ;; write ainfo_struct
     if ainfo_exists then begin
        if (DM_H5_WRITE_AINFO(h5_id,ainfo_struct,error_string) eq 1) then $
           begin
           PRINT,error_string
           DM_H5_CLOSE,h5_id
           return
        endif
     endif
     
     ;; append comments
     comments = [comments, STRJOIN(['DM_AVERAGE_H5, ',systime(), $
                                    ': Averaged from ', STRTRIM(count,2), $
                                    ' reconstructions.'])]

     if KEYWORD_SET(rm_phase_ramp) then begin
        comments = [comments,'   Removed phase ramps with DM_DO_PHASE_RAMP.']
     endif

     if KEYWORD_SET(high_pass) then begin
        comments = $
           [comments, $
            STRJOIN(['   Highpass-filtered with DM_DO_HIGH_PASS_FILTER, ', $
                     'width = ', STRTRIM(hp_width,2),' pixels and depth = ', $
                     STRTRIM(hp_depth,2),'.'])]
     endif
     
     if KEYWORD_SET(set_global_phase) then begin
        comments = [comments,'   Global phase set by DM_DO_GLOBAL_PHASE.']
     endif
     
     if KEYWORD_SET(align) then begin
        comments = [comments,'   Arrays aligned by DM_DO_CROSS_CORRELATION.']
     endif
     
     if (DM_H5_CREATE_COMMENTS(h5_id,specimen_name,collection_date,$
                               comments,error_string) eq 1) then begin
        PRINT,error_string
        ;DM_H5_CLOSE,h5_id
        ;return
     endif
     
     ;; write object
     if (DM_H5_WRITE_ITN(h5_id,itn_struct,avg_obj,recon_errors,$
                         error_string) ne 0) then begin
        PRINT,error_string
        DM_H5_CLOSE,h5_id
        return
     endif
     
     ;; close file
     DM_H5_CLOSE, h5_id
     
     ;; Produce tiff output
     if KEYWORD_SET(save_tiff) then begin
        if (status = $
            (DM_DO_HSV_TIFF(avg_obj,tiff_file=save_tiff, $
                            white_bkgrd=white_bkgrd)) ne 0) then begin 
           PRINT,'Error writing tiff '+save_tiff
        endif else begin
           PRINT,'Created tiff '+save_tiff
        endelse
     endif
     
  endif else begin
     print,STRJOIN(['DM_AVERAGE_H5: No matching files found in ', $
                    target_dir])
  endelse
END
