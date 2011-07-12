;+
; NAME:
;    DM_CENTER_PWR_SPEC
;
; PURPOSE:
;
;    This routine uses dm_do_center_pwr_spec to calculate the slopes
;    of the power spectra for a given diffraction pattern intensity,
;    in +/-X and +/-Y directions (namely; left half, right half, top
;    half and bottom half).  This can be used to roughly determine the
;    center position of the diffraction pattern, since the slopes of
;    oppositing halves will be similar.
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
;    dm_center_pwr_spec, filename [, step_size_pixels = step_size_pixels, 
;       /is_magnitude])
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    filename:  h5 file name
;
;
; INPUT KEYWORDS:
;
;    step_size_pixels:  Width of ring in which intensity is averaged.
;       If not specified, default is used.
;
;    /is_magnitude:  set if input array is magnitude (not intensity)
;
;
; MODIFICATION HISTORY:
;    2006-02-06 HM: written
;    2008-06-25 JN: - added keyword h5 to set if filename is hdf5 type
;                   - modified plotting
;                   - changed name to dm_ps_shift
;    2009-01-19 JN: - changed name to dm_center_pwr_spec
;                   - added dm_do_center_pwr_spec
;                   - added dm_do_power_spectrum
;                   - took out display of plot and saving as eps
;                   - added keyword /is_magnitude
;-

PRO dm_center_pwr_spec, filename, step_size_pixels=step_size_pixels, $
                        is_magnitude = is_magnitude

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage: dm_center_pwr_spec, filename'
     print, '[,step_size_pixels=step_size_pixels, /is_magnitude]' 
     print, ''
     print, ''
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
  
;; read adi
  IF (dm_h5_read_adi(h5_file_id,adi_struct,adi_array,$
                     adi_error_array,error_string) EQ 1) THEN BEGIN
     print,error_string
  ENDIF
  adi_cropped_struct = dm_do_adi_crop(adi_struct, adi_array, adi_error_array)
  adi_array = adi_cropped_struct.adi_array

  IF keyword_set(is_magnitude) THEN BEGIN
     diff_intensity = adi_array^2
  ENDIF ELSE BEGIN
     diff_intensity = adi_array
  ENDELSE

  nx = n_elements(diff_intensity[*,0])
  ny = n_elements(diff_intensity[0,*])

  diff_left = fltarr(nx, ny)
  diff_right = fltarr(nx, ny)
  diff_top = fltarr(nx, ny)
  diff_bot = fltarr(nx, ny)

  diff_left[0:nx/2-1,*] = diff_intensity[0:nx/2-1,*] 
  diff_right[nx/2:nx-1,*] = diff_intensity[nx/2:nx-1,*]
  diff_top[*,ny/2:ny-1] = diff_intensity[*,ny/2:ny-1]
  diff_bot[*,0:ny/2-1] = diff_intensity[*,0:ny/2-1]

  slope_left = dm_do_center_pwr_spec(diff_left, $
                                     step_size_pixels=step_size_pixels)
  print,'Slope_left = ', slope_left
  slope_right = dm_do_center_pwr_spec(diff_right, $
                                     step_size_pixels=step_size_pixels)
  print,'Slope_right = ', slope_right
  slope_top = dm_do_center_pwr_spec(diff_top, $
                                     step_size_pixels=step_size_pixels)
  print,'Slope_top = ', slope_top
  slope_bot = dm_do_center_pwr_spec(diff_bot, $
                                     step_size_pixels=step_size_pixels)
  print,'Slope_bot = ', slope_bot
 
  dm_h5_close, h5_file_id
;; change back if we have to 
  IF (strcmp(datafile_path,'.') EQ 0) THEN $
     cd,old_dir
END

