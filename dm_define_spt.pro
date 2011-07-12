;+
; NAME:
;    DM_DEFINE_SPT
;
; PURPOSE:
;
;    This program allows the user to define a support by clicking
;    around the autocorrelation of the adi_array. The support is
;    stored in the spt_array.
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
;    dm_define_spt, filename [,/square, /half, sigma=sigma]
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
;    SIGMA:  Defines width of filter.  If undefined then uses default
;        of 0.15.
;
;    SQUARE:  Makes a rectangular support from with sides half the
;        size of the rectangle made from 4 clicks by the user.
;
;    HALF:  Takes roi from user and rebins it to have the size then
;       pads it with zeros to return it to original array size.  This
;       allows the user to have a support with the shape of the
;       autocorrelation, but with half its dimensions. 
;
;    IS_DATA_CENTERED: Set this keyword to indicate that the arrays in
;       filename are already data-centered. Otherwise they are assumed
;       to be fft-centered and will be shifted accordingly in
;       subsequent functions.
;   
;
; MODIFICATION HISTORY:
;    2009-01-12 JN: written
;    2009-01-29 JN: added keyword /half.
;    2009-02-12 JFS: added keyword /is_data_centered
;    2010-04-08 JFS: removed unecessary lines of code
;-

PRO dm_define_spt, filename, square=square, sigma=sigma, half=half, help=help, $
                   is_data_centered=is_data_centered

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_define_spt, filename [,/square, /half, sigma=sigma, '
     PRINT, '        /is_data_centered]'
     print, 'Allows user to define a support from autocorrelation of the' 
     print, 'adi_array and save it as the spt_array.'
     print, 'Use keyword /square to make support a rectangle with sides half'
     print, 'the size of the rectangle made from 4 clicks by the user. Use the' 
     print, 'keyword /half to make a support half the size of the roi.'  
     print, 'If keyword sigma is not given the default is 0.15'
     RETURN
  ENDIF
  
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
  
;; read adi_array and structure                                                 
  IF (dm_h5_read_adi(h5_file_id, adi_struct, adi_array, adi_error_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF ELSE BEGIN
     print, 'Read adi_array'
  ENDELSE
  
;; check if spt_struct exists, otherwise create it
  IF (dm_h5_read_spt(h5_file_id,spt_struct,spt_array,error_string)$
      EQ 1) THEN BEGIN
     dm_init_spt_struct, spt_struct
     print, 'initialized spt_struct'
  ENDIF ELSE BEGIN
     response = ''
     window, 10
     tvscl, congrid(spt_array, 600,600)
     read, prompt='spt_array exists, do you wish to continue? [y/n] ', response
     IF (response EQ 'n') THEN BEGIN
        dm_h5_close, h5_file_id
        WDELETE, 10
        RETURN
     ENDIF
     WDELETE, 10
  ENDELSE
  
  adi_cropped_struct = dm_do_adi_crop(adi_struct, adi_array, adi_error_array, $
                                     is_data_centered=is_data_centered)
  data = adi_cropped_struct.adi_array
  data_size = size(data)
  nx = data_size[1]
  ny = data_size[2]
  
  support = BYTARR(1024,1024)
  
  autocorr = dm_do_autocorr(data, sigma=sigma,is_data_centered=is_data_centered)
  
;; Display autocorrelation and define support
  IF keyword_set(square) THEN BEGIN
     window,1, XSIZE=1024,YSIZE=1024,$
            title='CLick top, bottom, left, and right of autocorrelation'
     wset,1 
     tvscl, congrid(alog10(abs(autocorr)+.1e-5),1024,1024)
     print, 'Click top, bottom, left, and right of autocorrelation'
     cursor, x1,y1, /dev,/up
     print,'1'
     ;WAIT, 1
     cursor, x2,y2, /dev,/up
     print,'2'
     ;WAIT, 1
     cursor, x3,y3, /dev,/up
     print,'3'
     ;WAIT, 1
     cursor, x4,y4, /dev,/up
     print,'4'

     x_points = [x1,x2,x3,x4]
     y_points = [y1,y2,y3,y4]
     x_min = 512-(max(x_points)-min(x_points))/4
     x_max = 512+(max(x_points)-min(x_points))/4
     y_min = 512-(max(y_points)-min(y_points))/4
     y_max = 512+(max(y_points)-min(y_points))/4
     support[x_min:x_max, y_min:y_max] = 1.0
     spt_array = BYTE(congrid(support, nx, ny))
  ENDIF ELSE BEGIN
     window,1, XSIZE=1024,YSIZE=1024, title='Define support by clicking'
     wset,1 
     tvscl, congrid(alog10(abs(autocorr)+.1e-5),1024,1024)
     support_area = defroi(1024,1024)
     support[support_area] = 1.0
     spt_array = BYTE(congrid(support, nx, ny))
     IF keyword_set(half) THEN BEGIN
        half_spt = congrid(spt_array, nx/2, ny/2)
        spt_array = bytarr(nx,ny)
        spt_array[nx/4:nx*3/4-1, ny/4:ny*3/4-1] = half_spt
     ENDIF
  ENDELSE
  tvscl, congrid(spt_array, 1024, 1024)
  
;; Update spt_array
  response = ''
  read,prompt='Would you like to save this as your new support? [y/n] ',response
  IF (response EQ 'y') THEN BEGIN
;; update spt_array
     IF (dm_h5_write_spt(h5_file_id, spt_struct, spt_array, error_string)$
         EQ 1) THEN BEGIN
        print, error_string
        dm_h5_close, h5_file_id
        RETURN
     ENDIF ELSE BEGIN
        print, 'Updated "/spt" group.'
     ENDELSE
     
;; add comment
     comment_added = ['DM_DEFINE_SPT, '+systime()]
     IF (dm_h5_add_comments(h5_file_id,comment_added,error_string)$
         EQ 1) THEN BEGIN
        print, error_string
        dm_h5_close, h5_file_id
        RETURN
     ENDIF ELSE BEGIN
        print, 'Added comments'
     ENDELSE
  ENDIF ELSE BEGIN
     dm_h5_close, h5_file_id
     WDELETE, 1
     RETURN
  ENDELSE
  
  WDELETE, 1
  dm_h5_close, h5_file_id

  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END
