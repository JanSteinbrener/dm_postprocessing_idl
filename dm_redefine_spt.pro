;+
; NAME:
;    DM_REDEFINE_SPT
;
; PURPOSE:
;
;    This program allows the user to define a support by clicking
;    around the itn_array. The support is stored in the spt_array. If
;    the array is a 3D array, then the user will be asked to outline a
;    support for each slice of the array in z.
;
; AUTHOR:
;
;    Johanna Nelson
;    jnelson@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;    Reconstruction algorithms
;
;
; CALLING SEQUENCE:
;
;    dm_redefine_spt, filename[, start_z]
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
; OPTIONAL INPUT PARAMETERS:
;
;    start_z: If user passes a 3D array, then he can indicate a
;             starting index in z. The loop will start at that
;             slice. If it is not defined, 0 is assumed.
;
;
; INPUT KEYWORDS:
;
;    noskip: This keyword only is meaningful for a 3D array. When
;            invoked, the user will no longer be prompted to skip
;            slices. 
;
;
; MODIFICATION HISTORY:
;    2008-01-12 JN: written
;    2009-02-12 JFS: adi group no longer read-out
;    2009-08-04 JN: eliminated the zooming in
;    2010-01-20 JFS: 3D-fied, added start_z as optional input, added
;                    /noskip keyword.  
;- 

PRO dm_redefine_spt, filename, start_z, help=help, noskip=noskip
  
  if (KEYWORD_SET(help) or (N_PARAMS() lt 1)) then begin
     PRINT, 'Usage:  dm_redefine_spt, filename[,start_z]'
     PRINT, 'Allows user to define a new support from itn_array and' 
     PRINT, 'save it as the new spt_array'
     return
  endif
  
  ;; change directory if we have to
  datafile_path = FILE_DIRNAME(filename,/mark_directory)
  filename = FILE_BASENAME(filename)
  if (STRCMP(datafile_path,'.') eq 0) then $
     CD,datafile_path,current=old_dir

  ;; open .h5 intensity data                                                   
  if (DM_H5_OPENWRITE(filename, h5_file_id, error_string) eq 1) then begin
     PRINT, 'Error opening "'+filename+'" for writing'
     return
  endif
  PRINT, 'Opened file "'+filename+'"'
  
  if (DM_H5_READ_ITN(h5_file_id,itn_struct,itn_array,recon_errors,error_string)$
      eq 1) then begin
     PRINT, error_string
  endif

  ;; get size of itn
  itn_size = SIZE(itn_array,/dimensions) 
  nx = itn_size[0]
  ny = itn_size[1]

  ;; check if 3D
  if N_ELEMENTS(itn_size) eq 3 then begin
     nz = itn_size[2] 
     if N_ELEMENTS(start_z) eq 0 then $
        start_z = 0
  endif else begin 
     nz = 1
     start_z = 0
  endelse
  
  ;; check if spt_struct exists, otherwise create it
  if (DM_H5_READ_SPT(h5_file_id,spt_struct,spt_array,error_string)$
      eq 1) then begin
     DM_INIT_SPT_STRUCT, spt_struct
     PRINT, 'initialized spt_struct'
  endif
  
  ;; define new spt array and ask user what part of the itn array to
  ;; use 
  support = BYTARR(900,900)
  spt_array *= 0                ; reset spt_array to 0, converts to int! 

  WINDOW, /free, xsize=400, ysize=400, title='Magnitude'
  TVSCL, CONGRID(itn_array[*,*,nz/2], 400, 400)
  mag_win = !D.Window

  WINDOW, 21, xsize=400, ysize=400, title='Square'
  TVSCL, CONGRID(ABS(itn_array[*,*,nz/2])^2, 400, 400)
  square_win = !D.Window

  WINDOW, 22, xsize=400, ysize=400, title='Phase'
  TVSCL, CONGRID(ATAN(itn_array[*,*,nz/2], /phase), 400, 400)
  phase_win = !D.Window

  response = ''
  READ, prompt='Redefine support using "mag", "square", or "phase"? ', response

  WDELETE, mag_win
  WDELETE, square_win
  WDELETE, phase_win
  
  WINDOW, /free, xsize=900, ysize=900, title='Click around support'
  spt_win = !D.Window

  ;; convert itn_array to what user wants
  if STRCMP(response,'m',1,/fold_case) eq 1 then begin
     itn_array = ABS(itn_array)
  endif else if STRCMP(response,'s',1,/fold_case) eq 1 then begin
     itn_array = ABS(itn_array)^2
  endif else if STRCMP(response,'p',1,/fold_case) eq 1 then begin
     itn_array = ATAN(itn_array,/phase)
  endif else begin
     PRINT, 'Not a valid choice!'
     WDELETE, spt_win
     DM_H5_CLOSE, h5_file_id
     return
  endelse
  
  ;; loop through all slices
  i = start_z
  while i lt nz do begin
     ;; display current slice
     TVSCL, CONGRID(itn_array[*,*,i],900,900)
     

     response = ''
     if nz eq 1 then begin
        PRINT, 'Click around the new support region.'
        index = DEFROI(900, 900)
        
        support *= 0            ; start fresh
        support[index] = 1
        TVSCL, support
        
        while ((STRCMP(response,'y') ne 1) and $
               ((STRCMP(response,'n') ne 1))) do begin
           PRINT,'Save this as new support? [y/n]'
           response = GET_KBRD()
        endwhile
        if STRCMP(response,'y') eq 1 then begin
           spt_array[*,*] = CONGRID(support,nx,ny)
           write_spt = 1 
        endif else  begin 
           write_spt = 0
        endelse
        ;; this will exit the loop
        i = nz
     endif else begin
        ;; for 3D we ask if user wants to skip this slice unless
        ;; noskip is invoked
        response = ''
        if not KEYWORD_SET(noskip) then begin
           while ((STRCMP(response,'y') ne 1) and $
                  ((STRCMP(response,'n') ne 1))) do begin
              PRINT,'Skip this slice? [y/n]'
              response = GET_KBRD()
           endwhile
        endif
        if STRCMP(response,'y') eq 1 then begin
           i++
        endif else begin
           PRINT, 'Click around the new support region.'
           index = DEFROI(900, 900)
           
           support *= 0         ; start fresh
           support[index] = 1
           TVSCL, support
           response = ''
           while ((STRCMP(response,'y') ne 1) and $
                  ((STRCMP(response,'n') ne 1))) do begin
              PRINT,'Use this as support for slice '+STRTRIM(i,2)+'? [y/n]'
              response = GET_KBRD()
           endwhile
           if STRCMP(response,'y') eq 1 then begin
              ;; update spt_array
              spt_array[*,*,i] = CONGRID(support, nx, ny)
              i++
           endif else begin
              response = ''
              while ((STRCMP(response,'e') ne 1) and $
                     ((STRCMP(response,'r') ne 1))) do begin
                 PRINT,'Redo or Exit? [r/e] (Exit will prompt again for saving)'
                 response = GET_KBRD()
              endwhile
              if (STRCMP(response,'e') eq 1) then $
                 i = nz
              ;; nothing to do if we want to redo
           endelse
        endelse
     endelse
  endwhile
  
  ;; if nz > 1 then we have to ask user again about writing to the
  ;; file 
  if nz gt 1 then begin
     response = ''
     while ((STRCMP(response,'y') ne 1) and $
            ((STRCMP(response,'n') ne 1))) do begin
        PRINT,'Write new support to file? [y/n]'
        response = GET_KBRD()
     endwhile
     if STRCMP(response,'y') eq 1 then $
        write_spt = 1 else write_spt = 0
  endif

  ;; write if we have to
  if write_spt then begin
     ;; convert back to byte
     spt_array = BYTE(spt_array)
     if (DM_H5_WRITE_SPT(h5_file_id, spt_struct, spt_array, error_string) $
         eq 1) then begin
        PRINT, '[ERROR] DM_REDEFINE_SPT: '+error_string
        DM_H5_CLOSE, h5_file_id
        return
     endif else begin
        PRINT, 'Updated "/spt" group.'
     endelse
     
     ;; add comment
     comment_added = ['DM_REDEFINE_SPT, '+systime()]
     if (DM_H5_ADD_COMMENTS(h5_file_id,comment_added,error_string)$
         eq 1) then begin
        PRINT, error_string
        DM_H5_CLOSE, h5_file_id
        return
     endif else begin
        PRINT, 'Added comments'
     endelse
  endif
  
  DM_H5_CLOSE, h5_file_id
  WDELETE, spt_win
  
  ;; restore old directory and filename
  if (STRCMP(datafile_path,'.') eq 0) then begin
     CD,old_dir
     filename = STRJOIN([datafile_path,filename])
  endif

END
