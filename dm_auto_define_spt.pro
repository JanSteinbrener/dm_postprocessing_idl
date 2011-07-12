;+
; NAME:
;    DM_AUTO_DEFINE_SPT
;
; PURPOSE:
;
;    This program calculates the autocorrelation of the adi_array,
;    blurs it with the same gaussian function used in
;    dm_shrinkwrap.pro, and thresholds the array to determine the
;    support. The support is stored in the spt_array.
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
;    dm_auto_define_spt, filename (, threshold=threshold, sigma=sigma,
;       /half)
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
;    THRESHOLD:  Defines threshold to autocorrelation. If undefined
;        then uses default of 0.01.
;
;    HALF:  Set keyword to use autocorrelation which is rebinned to
;       half its original size and padded with zeros.
;
;    IS_DATA_CENTERED: Set this keyword to indicate that the arrays in
;       filename are already data-centered. Otherwise it is assumed
;       that they are fft-centered and they will be shifted accordingly.
;
; MODIFICATION HISTORY:
;    2008-01-15 JN: written
;    2009-02-12 JFS: added keyword is_data_centered
;-


PRO dm_auto_define_spt, filename, threshold=threshold, sigma=sigma, $
                        half=half, help=help, is_data_centered=is_data_centered

  IF (keyword_set(help_keyword) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage:  dm_auto_define_spt, filename '
     print, '[, threshold=threshold, sigma=sigma, half=half, /is_data_centered]'
     print, 'Thresholds autocorrelation (0.01 is default) and saves'
     print, 'result as support.  Sigma of autocorrelation is defaulted as 0.15'
     RETURN
  ENDIF

  IF NOT keyword_set(threshold) THEN threshold = 0.01

;; change directory if we have to
  datafile_base = File_Basename(filename,'.h5')
  datafile_path = File_Dirname(filename)
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,datafile_path,current=old_dir
  filename = datafile_base+'.h5'

;; open .h5 intensity data
  IF (dm_h5_openwrite(filename, h5_file_id, error_string) EQ 1) THEN BEGIN
     print, 'Error opening "'+filename+'" for reading'
     RETURN
  ENDIF
  print, 'Opened file "'+filename+'"'
  
  IF (dm_h5_read_adi(h5_file_id, adi_struct, adi_array, adi_error_array,$
                     error_string) EQ 1) THEN BEGIN
     print, error_string
  ENDIF
  
  ;; shift adi. NOTE: from here on the adi_array will be
  ;; data-centered! 
  if not KEYWORD_SET(is_data_centered) then $
     adi_array = SHIFT(adi_array,(SIZE(adi_array,/dimensions))/2)

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
                                     /is_data_centered)
  output_adi = adi_cropped_struct.adi_array
  adi_size = size(output_adi)
  nx = adi_size[1]
  ny = adi_size[2]
  IF adi_size[0] EQ 3 THEN BEGIN
     print, 'Array cannot be 3D.'
     RETURN
  ENDIF 

  autocorr = dm_do_autocorr(output_adi, sigma=sigma,/is_data_centered)
  
  window, 1, xsize=512, ysize=512
  tvscl, congrid(alog10(abs(autocorr)+.1e-5),512,512)

;; apply a gaussian blur before thresholding (identical to shrinkwrap)
  sigma=3
  range = round(3*sigma) ;; 2.3548*sigma is FWHM
  r = shift(dist(range*2+1), range, range)
  
  gaussian = exp(-(r/sigma)^2/2)
  gaussian_norm = gaussian/total(gaussian)
  autocorr = convol(autocorr, gaussian_norm, /center)

;; apply threshold
  support = (autocorr GE max(autocorr)*threshold)

  window, 2, xsize=512, ysize=512
  tvscl, congrid(support,512,512)

  IF keyword_set(half) THEN BEGIN
     half_autocorr = congrid(support, nx/2, ny/2)
     support = bytarr(nx,ny)
     support[nx/4:nx*3/4-1, ny/4:ny*3/4-1] = half_autocorr
     wset, 2
     tvscl, congrid(support,512,512)
  ENDIF

;; Update spt_array
  response = ''
  read, prompt='Would you like to save this as your new support? [y/n] ', $
        response
  IF (response EQ 'y') THEN BEGIN
     IF (dm_h5_write_spt(h5_file_id, spt_struct, support, error_string)$
         EQ 1) THEN BEGIN
        print, error_string
        dm_h5_close, h5_file_id
        RETURN
     ENDIF ELSE BEGIN
        print, 'Updated "/spt" group.'
     ENDELSE

;; add comment
     str_threshold = STRTRIM(STRING(threshold),1)
     comment_added = ['DM_AUTO_DEFINE_SPT, '+systime()+': with threshold ' $
                      +str_threshold+'.']
     IF (dm_h5_add_comments(h5_file_id,comment_added,error_string)$
         EQ 1) THEN BEGIN
        print, error_string
        dm_h5_close, h5_file_id
        WDELETE, 1
        WDELETE, 2
        RETURN
     ENDIF ELSE BEGIN
        print, 'Added comments'
     ENDELSE
  ENDIF

  WDELETE, 1
  WDELETE, 2
  dm_h5_close, h5_file_id
  
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir

END
