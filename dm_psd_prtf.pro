;+
; NAME:
;    DM_PSD_PRTF
;
; PURPOSE:
;
;    This program calls dm_do_psd_prtf to calculate the average
;    intensity of an adi_array at specified rings of spatial
;    frequency. The ccd distance, ccd pixel size, and wavelength are
;    read off the adi_struct if they are found to be greater than
;    zero, otherwise, they are assumed to be 12.8 cm, 20 microns, and
;    1.7 nm respectively. The resulting plot is saved as an eps.
;
; AUTHOR:
;
;    Jan Steinbrener
;    jan@xray1.physics.sunysb.edu
;
;
; CATEGORY:
;
;    Reconstruction algorithms: dm_recon
;
;
; CALLING SEQUENCE:
;
;    dm_psd_prtf, filename_array [, label_array,
;       step_size_pixels=step_size_pixels,
;       eps_file=eps_file,
;       is_magnitude=is_magnitudes, 
;       is_data_centered=is_data_centered,
;       no_labels=no_labels,
;       plot_itn=plot_itn, 
;       only_plot_itn=only_plot_itn])
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    filename_array:  An array of h5 file names
;
;
; OPTIONAL INPUT PARAMETERS:
;
;    label_array: An array of labels to be printed on the plot
;
;
; INPUT KEYWORDS:
;
;    step_size_pixels:  Width of ring in which intensity is averaged.
;       If not specified, default is used.
;
;    eps_file:  Default is "filename_pwr_spec.eps"
;
;    is_magnitude:  set if input array is magnitude (not intensity)
;
;    is_data_centered: set this keyword to indicate that the array in
;       filename is already data-centered. Otherwise it is assumed to
;       be fft-centered and will be shifted in subsequent functions.
;
;    no_labels: set this keyword if you do not want the program to add
;               labels to the plot.
;
;    plot_itn:  also calculates and plots the average intensity of the
;       itn_array
;
;    only_plot_itn: only calculates and plots the average intensity of 
;                   the itn_array
;
; MODIFICATION HISTORY:
;    2010-02-04 JFS: created
;
;-
;=======================================================
;;  Used by dm_psd_prtf to plot the spatial frequency and half-period on
;;  the same axis

FUNCTION HalfPeriodFormat, axis, index, value
  
  RETURN, String(0.5/value * 1000, Format='(I3)') 
; Format as an integer.
END 


;=======================================================
;;  Used by dm_PSD_PRTF to do the actual plotting

PRO DM_PSD_PRTF_PLOT, n_files, q_inverse_um_array, psd_structs, prtf_structs, $
                      alt_psd_structs, q_min, q_max, prtf_min, psd_min, psd_max, $
                      colors, label_array, label_positions

  ;; first plot PRTF on non-log scale!
  for i=0, n_files-1 do begin
     if i eq 0 then begin
        PLOT, [q_min,q_max], [0,1], xstyle=9, xtitle='frequency (1/um)', $
              /xlog, position=[0.08,0.15,0.80,0.85], charsize=1.25, $
              ystyle=4,/nodata
        AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
              XTickFormat= 'HalfPeriodFormat', $
              xrange=[q_min,q_max], /xlog, $
              XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save
        AXIS, yaxis=1, ytitle='PRTF', charsize=1.25, ystyle=1, $
              yrange=[prtf_min,1], /save
        OPLOT, *(q_inverse_um_array[i]), (*prtf_structs[i]).prtf_array, $
               color=colors[i], linestyle=2
     endif else begin
        OPLOT, *(q_inverse_um_array[i]), (*prtf_structs[i]).prtf_array, $
               color=colors[i]
     endelse    
     if not KEYWORD_SET(no_labels) then $
        XYOUTS, 0.88,label_positions[i],label_array[i],color=colors[i],/normal
  endfor
  
  ;; in the second round, plot the PSD curves on a y-log scale
  for i=0, n_files-1 do begin
     if i eq 0 then begin
        AXIS, yaxis=0, ytitle='PSD', charsize=1.25, ystyle=1, $
              yrange=[psd_min,psd_max], /save, /ylog
        OPLOT, *(q_inverse_um_array[i]), (*psd_structs[i]).power_spec, $
               color=colors[i]
        if N_ELEMENTS(alt_psd_structs) ne 0 then begin
           OPLOT, *(q_inverse_um_array[i]), (*alt_psd_structs[i]).power_spec, $
                  color=colors[i], linestyle=1
        endif
     endif else begin
        OPLOT, *(q_inverse_um_array[i]), (*psd_structs[i]).power_spec, $
               color=colors[i]
        if N_ELEMENTS(alt_psd_structs) ne 0 then begin
           OPLOT, *(q_inverse_um_array[i]), (*alt_psd_structs[i]).power_spec, $
                  color=colors[i], linestyle=1
        endif
     endelse    
  endfor
END 

;=======================================================

PRO DM_PSD_PRTF_CLEANUP, n_files, psd_structs, alt_psd_structs, $
                         q_inverse_um_array, prtf_structs
  
  for i=0,n_files -1 do begin
     if PTR_VALID(psd_structs[i]) then $
        PTR_FREE, psd_structs[i]
     
     if PTR_VALID(q_inverse_um_array[i]) then $
        PTR_FREE, q_inverse_um_array[i]
  endfor
  
  if N_ELEMENTS(alt_psd_structs) ne 0 then begin
     for i=0,n_files -1 do begin
        if PTR_VALID(alt_psd_structs[i]) then $
           PTR_FREE, alt_psd_structs[i]
     endfor
  endif
  
END

;=======================================================

PRO DM_PSD_PRTF, filename_array, label_array, $
                 step_size_pixels=step_size_pixels, $
                 eps_file=eps_file, $
                 is_magnitude=is_magnitude, $
                 is_data_centered=is_data_centered, $
                 no_labels=no_labels, plot_itn=plot_itn, $
                 only_plot_itn=only_plot_itn
  
  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     PRINT, 'Usage: DM_PSD_PRTF, filename_array [,label_array, '
     PRINT, 'eps_file=eps_file, is_magnitude=is_magnitude, '
     PRINT, 'is_data_centered=is_data_centered, no_labels=no_labels'
     PRINT, 'plot_itn=plot_itn, only_plot_itn=only_plot_itn]'
     PRINT, 'Calculates and plots the power spectrum of one or more '
     PRINT, 'files, either of the adi, the itn or both.'
     PRINT, 'The ccd distance, ccd pixel size, and wavelength are read off'
     PRINT, 'each files adi_struct if they are found to be greater than zero,'
     PRINT, 'otherwise, they are assumed to be to be 12.8 cm, 20 microns, '
     PRINT, 'and 1.7 nm respectively.'
     return
  ENDIF

  ;; determine number of files, check input variables
  n_files = N_ELEMENTS(filename_array)

  if not KEYWORD_SET(no_labels) then begin
     if N_ELEMENTS(label_array) eq 0 then begin
        label_array = STRARR(n_files)
        for i=0,n_files-1 do begin
           label_array[i] = STRJOIN(['File ',STRTRIM(i,2)])
        endfor
     endif else begin
        if N_ELEMENTS(label_array) ne n_files then begin
           PRINT, '[ERROR] DM_PSD_PRTF: label_array must have as many '+ $
                  'entries as filename_array!'
           return
        endif
     endelse
  endif
  
  ;; check/create eps filename
  filedir = FILE_DIRNAME(filename_array[0])
  filebase = FILE_BASENAME(filename_array[0],'.h5')
  path_sep = PATH_SEP()
  if (SIZE(eps_file,/type) ne 7) then $
     eps_file = STRJOIN([filedir,path_sep,filebase,'_PSD_PRTF.eps'])

  ;; initialize arrays to store info from each file
  psd_structs = PTRARR(n_files)
  prtf_structs = PTRARR(n_files)
  q_inverse_um_array = PTRARR(n_files)
  if KEYWORD_SET(plot_itn) then $
     alt_psd_structs = PTRARR(n_files)

  ;; now populate the arrays
  for i=0,n_files -1 do begin
     if (status = DM_H5_OPENREAD(filename_array[i], $
                                 this_h5_id, error_string) ne 0) then begin
        PRINT, '[ERROR] DM_PSD_PRTF: '+error_string
        DM_PSD_PRTF_CLEANUP,n_files,psd_structs,q_inverse_um_array, prtf_structs
        return
     endif
     
     ;; adi array
     if (DM_H5_READ_ADI(this_h5_id, this_adi_struct, this_adi_array, $
                        this_adi_error_array, error_string) ne 0) then begin
        PRINT, '[ERROR] DM_PSD_PRTF: '+error_string
        DM_H5_CLOSE, this_h5_id
        DM_PSD_PRTF_CLEANUP,n_files,psd_structs,q_inverse_um_array, prtf_structs
        return
     endif
     this_cropped_struct = DM_DO_ADI_CROP(this_adi_struct, this_adi_array, $
                                          this_adi_error_array, $
                                          is_data_centered=is_data_centered)     
     this_adi_array = this_cropped_struct.adi_array

     ;; read itn_array
     if (DM_H5_READ_ITN(this_h5_id,itn_struct,this_itn_array,$
                        these_itn_errors,error_string) eq 1) then begin
        PRINT, '[ERROR] DM_PSD_PRTF: '+error_string
        DM_H5_CLOSE, this_h5_id
        DM_PSD_PRTF_CLEANUP,n_files,psd_structs,q_inverse_um_array, prtf_structs
        return
     endif
     ;this_itn_array = ABS(BH_SFFT(this_itn_array, /double, /preserve_power))

      ;; check sizes
     itn_dims = SIZE(this_itn_array,/dimensions)
     adi_dims = SIZE(this_adi_array,/dimensions)
     if ((itn_dims[0] ne adi_dims[0]) and (itn_dims[1] ne adi_dims[1])) then begin
        PRINT, '[ERROR] DM_PRTF: ' + $
               'cropped adi_array and itn_array must be of same size!'
        DM_H5_CLOSE, this_h5_id
        DM_PRTF_CLEANUP,n_files,prtf_structs,q_inverse_um_array
        return
     endif

     ;; close file
     DM_H5_CLOSE, this_h5_id

     ;; check metadata
     if this_adi_struct.camera_z_meters gt 0.0 then begin
        this_ccd_z_um = this_adi_struct.camera_z_meters*1.e6
     endif else begin
        PRINT, $
           '[WARNING] DM_PSD_PRTF: ccd distance not found, using default'
        this_ccd_z_um = 128000.
     endelse
     if this_adi_struct.camera_x_pixelsize_meters gt 0.0 then begin
        this_ccd_pixelsize_um = this_adi_struct.camera_x_pixelsize_meters*1.e6
     endif else begin
        PRINT, $
          '[WARNING] DM_PSD_PRTF: ccd pixel size not found, using default'
        this_ccd_pixelsize_um = 20. 
     endelse
     if this_adi_struct.lambda_meters gt 0.0 then begin
        this_wavelength_um = this_adi_struct.lambda_meters*1.e6
     endif else begin
        PRINT, $
           '[WARNING] DM_PSD_PRTF: wavelength not found, using default'
        this_wavelength_um = 0.0017
     endelse
     
     ;; calculate PSD of adi or itn or both
     if not KEYWORD_SET(only_plot_itn) then begin
        this_psd_struct = $
           DM_DO_POWER_SPECTRUM(this_adi_array, $
                             step_size_pixels=step_size_pixels, $
                             is_magnitude=is_magnitude, $
                             is_data_centered=is_data_centered)
        psd_structs[i] = PTR_NEW(this_psd_struct)
     endif else begin
        this_psd_struct = $
           DM_DO_POWER_SPECTRUM(ABS(BH_SFFT(this_itn_array, $
                                            /double, /preserve_power)), $
                                step_size_pixels=step_size_pixels, $
                                /is_magnitude, /is_data_centered)
        psd_structs[i] = PTR_NEW(this_psd_struct)
     endelse
     if KEYWORD_SET(plot_itn) then begin
        alt_psd_structs[i] = $
           PTR_NEW(DM_DO_POWER_SPECTRUM(ABS(BH_SFFT(this_itn_array, $
                                                    /double, /preserve_power)), $
                                        step_size_pixels=step_size_pixels, $
                                        /is_magnitude, /is_data_centered))
     endif 

     ;; calculate PRTF
     this_prtf_struct = DM_DO_PRTF(this_adi_array,this_itn_array, $
                                   /is_real, is_data_centered=is_data_centered)
     
     prtf_structs[i] = PTR_NEW(this_prtf_struct)

     this_q = FINDGEN(N_ELEMENTS(this_psd_struct.power_spec))
     q_inverse_um_array[i] = $
        PTR_NEW(this_q*this_ccd_pixelsize_um/ $
                (this_wavelength_um*this_ccd_z_um))
  endfor


  ;; determine min/max q range and min/max PSD and PRTF values
  q_max = 1.
  q_min = 0
  psd_min = 10.e8
  psd_max = 0.1
  prtf_min = 1.
  for i=0, n_files-1 do begin
     this_nobs_i = (*psd_structs[i]).nobeamstop_i
     this_q_max = MAX(*(q_inverse_um_array[i]))
     this_q_min = (*q_inverse_um_array[i])[this_nobs_i]
     this_prtf_min = MIN((*prtf_structs[i]).prtf_array)
     if KEYWORD_SET(plot_itn) then begin
        this_psd_max = MAX([MAX(((*psd_structs[i]).power_spec)[this_nobs_i:*]), $
                            MAX(((*alt_psd_structs[i]).power_spec)[this_nobs_i:*])])
        this_psd_min = MIN([MIN(((*psd_structs[i]).power_spec)[this_nobs_i:*]), $
                            MIN(((*alt_psd_structs[i]).power_spec)[this_nobs_i:*])])
     endif else begin
        this_psd_min = MIN(((*psd_structs[i]).power_spec)[this_nobs_i:*])
        this_psd_max = MAX(((*psd_structs[i]).power_spec)[this_nobs_i:*])
     endelse
     if this_q_max gt q_max then $
        q_max = this_q_max
     if this_q_min gt q_min then $
        q_min = this_q_min
     if this_prtf_min lt prtf_min then $
        prtf_min = this_prtf_min
     if this_psd_max gt psd_max then $
        psd_max = this_psd_max
     if this_psd_min lt psd_min then $
        psd_min = this_psd_min
  endfor
  ;; add 10 to show a little more
  q_max += 10

  ;; calculate label positions
  if not KEYWORD_SET(no_labels) then begin
     label_positions = FLTARR(n_files)
     decrement = ((0.69/n_files) lt 0.1) ? (0.69/n_files) : 0.1
     for i=0,n_files-1 do begin
        label_positions[i] = 0.84 - i*decrement
     endfor
  endif

  ;; create colors as indices into the color table 
  ;; that is loaded further down.
  n_colors = n_files
  colors = INTARR(n_colors)
  for i=0,n_colors-1 do begin
     colors[i] = 255 - i*(255/n_colors)
  endfor

  ;; plot in window
  WINDOW,/free, xsize=1000,ysize=600
  DEVICE, decomposed = 0
  LOADCT, 39
  DM_PSD_PRTF_PLOT, n_files, q_inverse_um_array, psd_structs, prtf_structs, $
                    alt_psd_structs, q_min, q_max, prtf_min, psd_min, psd_max, $
                    colors, label_array, label_positions
  
  ;; generate PS file
  old_plot = !D.name
  old_font = !P.font
  SET_PLOT, 'ps'
  DEVICE, file = eps_file, xsize = 10., ysize = 6., yoffset = 1., /inch, /color

  ;; this has been introduced in 7.1
  if (FLOAT(!version.release) ge 7.1) then $
     DEVICE, decomposed=0
  
  !P.font = 0

  ;; load color table Rainbow + black
  LOADCT, 40

  DM_PSD_PRTF_PLOT, n_files, q_inverse_um_array, psd_structs, prtf_structs, $
                    alt_psd_structs, q_min, q_max, prtf_min, psd_min, psd_max, $
                    colors, label_array, label_positions

  DEVICE, /close
  PRINT, 'DM_PSD_PRTF: wrote eps file "'+eps_file+'"'
  SET_PLOT, old_plot
  !P.font = old_font  
  LOADCT, 0
  
  ;; free the pointer arrays
  DM_PSD_PRTF_CLEANUP, n_files, psd_structs, alt_psd_structs, $
                       q_inverse_um_array, prtf_structs
END
