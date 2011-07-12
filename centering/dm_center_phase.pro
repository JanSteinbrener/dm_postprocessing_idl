;+
; NAME:
;    DM_CENTER_PHASE
;
; PURPOSE:
;
;    This program shifts the center of the FFT of the itn_array by
;    xshift_pixels and yshift_pixels and displays phase of real space
;    object. The center position with the least phase change is a good
;    estimate of the true diffraction center.
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
;    dm_center_phase, filename, xshift_pixels, yshift_pixels
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
;    xshift_pixels: Number of pixels to shift center (left is
;        negative)
;
;    yshift_pixels:  Number of pixels to shift center (down is
;        negative)
;
; INPUT KEYWORDS:
;
;    none
;
; MODIFICATION HISTORY:
;    2009-01-19 JN: written
;-
PRO dm_center_phase, filename, xshift_pixels, yshift_pixels, help=help

  IF (keyword_set(help) OR (n_params() LT 3)) THEN BEGIN
     print, 'Usage:  dm_center_phase, filename, xshift_pixels, yshift_pixels'
     print, 'Shifts center of FFT of itn_array by xshift_pixels and' 
     print, 'yshift_pixels and displays phase of real space object.'
     print, 'The center position with the least phase change is a good' 
     print, 'estimate of the true diffraction center.'
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

  IF (dm_h5_read_itn(h5_file_id,itn_struct,itn_array,$
                     error_string) EQ 1) THEN BEGIN
     print,error_string
  ENDIF

  array_size = size(itn_array)
  nx = array_size[1]
  ny = array_size[2]
  
  fourier = bh_sfft(itn_array, preserve_power=preserve_power)
  shifted_fft = shift(fourier, xshift_pixels, yshift_pixels)
  object = bh_sfft(shifted_fft, /inverse, preserve_power=preserve_power)
  
  phase = atan(imaginary(object), real_part(object), /phase)
  window, 1, title='Phase after shifted FFT', xsize=nx/2, ysize=ny/2
  tvscl, congrid(phase, nx/2, ny/2)
  
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
  
  dm_h5_close,h5_file_id
END
