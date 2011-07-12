;+
; NAME:
;    DM_PRTF
;
; PURPOSE:
;
;    This program takes the itn_array and adi_array and uses the function
;    dm_do_prtf to plot the PRTF and saves the plot as an eps file.
;    The ccd distance, ccd pixel size, and wavelength are read off the
;    adi_struct if they are found to be greater than zero, otherwise, they
;    are assumed to be 12.8 cm, 20 microns, and 1.7 nm respectively.
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
;    dm_PRTF, filename[, label_array, eps_file = eps_file, 
;             is_data_centered=is_data_centered, 
;             no_labels=no_labels]
;
; RETURN VALUE:
;
;    none
;
;
; INPUT PARAMETERS:
;
;    filename_array:  An array of names of .h5 files of which one
;        wants to compute the PRTFs. This can also just be one file.
;
;
; OPTIONAL INPUT PARAMETERS:
; 
;    label_array: An array of labels for each PRTF
;    mtf_resolutions_nm:  An array of MTF Rayleigh resolutions to be plotted
;
;
; INPUT KEYWORDS:
;
;    eps_file:  If not specified default file name is
;        "filename_PRTF.eps"
;
;    /is_data_centered: set this keyword if the incoming adi_array is
;        already data-centered. Otherwise it is assumed to be
;        fft-centered and will be shifted accordingly
;
;    /no_labels: set this keyword to suppress labels in the generated 
;        plot. If not the user defined labels label_array will be used
;        or labels will be automatically generated if not supplied
;
;    /wiener_filter: set this keyword to also compute a Wiener-filter
;        based on the PSD of the reconstructed itn_array and apply it
;        to the computed PRTF curve. 
;
;
; MODIFICATION HISTORY:
;    2009-01-15 JN: written
;    2009-01-22 JN: read needed data off adi_struct
;    2009-01-22 JFS: updated to work with updated dm_do_prtf
;    2009-02-12 JFS: added keyword /is_data_centered
;    2009-06-17 JFS: added recon_errors to dm_h5_read_itn
;    2009-06-18 JFS: added possibility to display multiple PRTFs
;                    in one plot with optional labels.
;    2009-06-24 JN: added mtf option
;               JFS: now checks for IDL version to make sure 
;                    that decomposition is turned off for 7.1 and
;                    above in PS plots.
;               JFS: added new procedure dm_prtf_cleanup for cleanup
;    2009-07-12 JFS: changed default for CCD z distance to 12.8 cm.
;    2010-04-05 JFS: added /wiener_filter keyword
;    2010-07-05 JFS: fixed wrong calc of Wiener filter (no squaring
;                    necessary) and calc of real signal
;-

;=======================================================
;;  Used by dm_PRTF to plot the spatial frequency and half-period on
;;  the same axis

FUNCTION HalfPeriodFormat, axis, index, value

  RETURN, String(0.5/value * 1000, Format='(I3)') 
; Format as an integer.
END 

;=======================================================
;; Used to calculate the Wiener filter
PRO dm_prtf_wiener, itn_array, q_inverse_um_array, filename, wiener_filter, $
                    step_size_pixels=step_size_pixels

  itn_array = ABS(BH_SFFT(itn_array, /double, /preserve_power))
  psd_struct = DM_DO_POWER_SPECTRUM(itn_array, $
                                    step_size_pixels=step_size_pixels, $
                                    /is_data_centered,/is_magnitude)
  
  ;; determine maximum q range and min/max PSD value
  q_min = 0.1
  q_max = MAX(q_inverse_um_array)+10
  ;print,q_max
  psd_min = MIN(psd_struct.power_spec)
  psd_max = MAX(psd_struct.power_spec)

  ;; plot the PSD and have user click on slope and noise floor
  WINDOW, /free, xsize=800, ysize=600, $
          title='Power spectral density and Wiener filter'
  this_win = !D.Window
  DEVICE, decomposed = 0
  LOADCT, 39
  ok = 0  
  while ok eq 0 do begin
     PRINT, 'DM_WPRTF: Click on two points to define slope'
     PLOT, q_inverse_um_array, psd_struct.power_spec, xstyle=9, $
           xtitle='frequency (1/um)', ytitle='PSD', $
           charsize=1.25, xrange=[q_min,q_max], yrange=[psd_min, psd_max], $
           ystyle=1, /ylog, /xlog, position=[0.1,0.1,0.9,0.9],color=255
     AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
           XTickFormat= 'HalfPeriodFormat', $
           xrange=[q_min,q_max],$
           XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save,/xlog
     CURSOR, x1, y1,/down
     CURSOR, x2, y2,/down
     slope = (ALOG10(y2)-ALOG10(y1))/(ALOG10(x2)-ALOG10(x1))
     offset = ALOG10(y1)-slope*ALOG10(x1)
     signal = q_inverse_um_array^slope*10^(offset)
     OPLOT, q_inverse_um_array, signal, color=180
     
     PRINT,'Is the fit OK?'
     answer = ' '
     while (STRCMP(answer,'y') or STRCMP(answer,'n')) ne 1 do begin
        PRINT, 'Please answer (y/n).'
        answer = GET_KBRD()
        if STRCMP(answer,'y') eq 1 then $
           ok = 1 else ok = 0
     endwhile
  endwhile

  ok = 0
  while ok eq 0 do begin
     PLOT, q_inverse_um_array, psd_struct.power_spec, xstyle=9, $
           xtitle='frequency (1/um)', ytitle='PSD', $
           charsize=1.25, xrange=[q_min,q_max], yrange=[psd_min, psd_max], $
           ystyle=1, /xlog, /ylog, position=[0.1,0.1,0.9,0.9],color=255
     AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
           XTickFormat= 'HalfPeriodFormat', $
           xrange=[q_min,q_max],$
           XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save, /xlog
     OPLOT, q_inverse_um_array, signal, color=180
     PRINT,'DM_WPRTF: Click on noise floor.'
     CURSOR,x1,noise,/down
     OPLOT,q_inverse_um_array, $
           REPLICATE(noise,N_ELEMENTS(q_inverse_um_array)), color=90
     PRINT,'Are you happy with the noise floor?'
     answer = ' '
     while (STRCMP(answer,'y') or STRCMP(answer,'n')) ne 1 do begin
        PRINT, 'Please answer (y/n).'
        answer = GET_KBRD()
        if STRCMP(answer,'y') eq 1 then $
           ok = 1 else ok = 0
     endwhile
  endwhile

  ;; calculate true_signal
  true_signal = signal - noise
  neg_inds = WHERE(true_signal lt 0, neg_count)
  if neg_count ne 0 then $
     true_signal[neg_inds] = 0.0

  ;; we have signal and noise, now let's compute the Wiener
  ;; filter. Note that we don't need to square since the PSD is
  ;; already proportional to the square of these quantities, see
  ;; e.g. Eq. 13.3.7 in Numerical Recipes book.
  wiener_filter = true_signal/(true_signal+noise)
  ;; make sure it is finite
  ndef_i = WHERE(~FINITE(wiener_filter),ndef_count)
  if ndef_count ne 0 then $
     wiener_filter[ndef_i] = 1

  PLOT, [q_min,q_max], [0,1], xstyle=9, xtitle='frequency (1/um)', $
        /xlog, position=[0.1,0.1,0.9,0.9], charsize=1.25, $
        ystyle=4,/nodata
  AXIS, yaxis=1, ytitle='Wiener filter', charsize=1.25, ystyle=1, $
        yrange=[0,1], /save
  OPLOT,q_inverse_um_array,wiener_filter
  AXIS, yaxis=0, ytitle='PSD', charsize=1.25, ystyle=1, $
        yrange=[psd_min,psd_max], /save, /ylog
  OPLOT, q_inverse_um_array, psd_struct.power_spec, $
         color=255
  OPLOT, q_inverse_um_array, signal, color=180
  OPLOT,q_inverse_um_array, $
        REPLICATE(noise,N_ELEMENTS(q_inverse_um_array)), color=90
  AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
        XTickFormat= 'HalfPeriodFormat', $
        xrange=[q_min,q_max],$
        XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save, /xlog

  ;; check if ok
  PRINT,'Accept Wiener-filter?'
  answer = ' ' 
  while (STRCMP(answer,'y') or STRCMP(answer,'n')) ne 1 do begin
     PRINT, 'Please answer (y/n).'
     answer = GET_KBRD()
     if STRCMP(answer,'n') eq 1 then begin
        wiener_filter = REPLICATE(1,N_ELEMENTS(q_inverse_um_array))
     endif else begin
        ;; write out as .eps
        filedir = FILE_DIRNAME(filename)
        filebase = FILE_BASENAME(filename,'.h5')
        path_sep = PATH_SEP()
        
        eps_file = STRJOIN([filedir,path_sep,filebase,'_wiener.eps'])
        
        old_plot = !D.name
        old_font = !P.font
        SET_PLOT, 'ps'
        DEVICE, file = eps_file, xsize = 6., ysize = 4., yoffset = 1., $
                /inch, /color
        
        ;; this has been introduced in 7.1
        if (FLOAT(!version.release) ge 7.1) then $
           DEVICE, decomposed=0
        
        !P.font = 0
        
        ;; load color table Rainbow + black
        LOADCT, 40
        ;print,psd_min,psd_max
        PLOT, [q_min,q_max], [0,1], xstyle=9, xtitle='frequency (1/um)', $
              /xlog, position=[0.1,0.1,0.9,0.9], charsize=1.25, $
              ystyle=4,/nodata
        AXIS, yaxis=1, ytitle='Wiener filter', charsize=1.25, ystyle=1, $
              yrange=[0,1], /save
        OPLOT,q_inverse_um_array,wiener_filter
        AXIS, yaxis=0, ytitle='PSD', charsize=1.25, ystyle=1, $
              yrange=[psd_min,psd_max], /save, /ylog
        OPLOT, q_inverse_um_array, psd_struct.power_spec, $
               color=255
        OPLOT, q_inverse_um_array, signal, color=180
        OPLOT,q_inverse_um_array, $
              REPLICATE(noise,N_ELEMENTS(q_inverse_um_array)), color=90
        AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
              XTickFormat= 'HalfPeriodFormat', $
              xrange=[q_min,q_max],$
              XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save, /xlog

        DEVICE, /close
        PRINT, 'DM_PRTF: wrote eps file "'+eps_file+'"'
        SET_PLOT, old_plot
        !P.font = old_font  
        LOADCT, 0
     endelse
  endwhile
  WDELETE, this_win
END 

;=======================================================
;;  Used by dm_PRTF to do the actual plotting

PRO dm_prtf_plot, n_files, q_inverse_um_array, prtf_structs, $
                  q_max, prtf_min, colors, label_array, label_positions, $
                  n_mtfs, mtf_curves, mtf_freq, mtf_label_array, $
                  wiener_filter=wiener_filter
 
  if KEYWORD_SET(wiener_filter) then $
     ytitle = 'wPRTF' else ytitle = 'PRTF'
  for i=0, n_files-1 do begin
     if i eq 0 then begin
        PLOT, *(q_inverse_um_array[i]), (*prtf_structs[i]).prtf_array, xstyle=9, $
              xtitle='frequency (1/um)', ytitle=ytitle, $
              position=[0.15,0.15,0.9,0.85], charsize=1.25, xrange=[1,q_max], $
              yrange=[prtf_min,1.0],ystyle=1, /xlog,color=colors[i]
        AXIS, xaxis=1.0, xtitle='half-period (nm)', charsize=1.25, xstyle=1, $
              XTickFormat= 'HalfPeriodFormat', $
              xrange=[1,q_max],$
              XTickV=[10, 16.5, 25, 50, 100], xticks=5, /save, /xlog
     endif else begin
        OPLOT, *(q_inverse_um_array[i]), (*prtf_structs[i]).prtf_array, $
               color=colors[i]
     endelse    
     if not KEYWORD_SET(no_labels) then $
        XYOUTS, 0.92,label_positions[i],label_array[i],color=colors[i],/normal
  endfor
  
  if n_mtfs GT 0 then begin
     for i=0, n_mtfs-1 do begin
        OPLOT, *(mtf_freq[i]), *(mtf_curves[i]), color=colors[i], linestyle=2
        
        if not KEYWORD_SET(no_labels) then $
           XYOUTS, 0.92,label_positions[i+n_files],mtf_label_array[i], $
                   color=colors[i], /normal
     endfor
  endif
END 

;=======================================================

PRO DM_PRTF_CLEANUP, n_files, prtf_structs, q_inverse_um_array, $
                     n_mtfs, mtf_curves, mtf_freq

  for i=0,n_files -1 do begin
     if PTR_VALID(prtf_structs[i]) then $
        PTR_FREE, prtf_structs[i]
     
     if PTR_VALID(q_inverse_um_array[i]) then $
        PTR_FREE, q_inverse_um_array[i]
  endfor
  
  if (N_ELEMENTS(n_mtfs) ne 0) then begin
     for i=0, n_mtfs-1 do begin
        if PTR_VALID(mtf_curves[i]) then $
           PTR_FREE, mtf_curves[i]
        if PTR_VALID(mtf_freq[i]) then $
           PTR_FREE, mtf_freq[i]
     endfor
  endif

END

;=======================================================

PRO DM_PRTF, filename_array, label_array, $
             eps_file=eps_file, $
             is_data_centered=is_data_centered, $
             no_labels=no_labels, $
             mtf_resolutions_nm=mtf_resolutions_nm, $
             wiener_filter=wiener_filter

  IF (keyword_set(help) OR (n_params() LT 1)) THEN BEGIN
     print, 'Usage: DM_PRTF, filename_array [,label_array, eps_file=eps_file$'
     print, 'mtf_resolutions_nm=mtf_resolutions_nm, /wiener_filter]'
     print, 'Takes the itn_array and adi_array of all files in filename_array' 
     print, 'and plots their PRTFs in one plot with optional labels.'
     print, 'Also saves the plot as an eps file.'
     print, 'Plots mtf curves for given mtf resolutions.'
     print, 'The ccd distance, ccd pixel size, and wavelength are read off'
     print, 'each files adi_struct if they are found to be greater than zero,'
     print, 'otherwise, they are assumed to be to be 12.8 cm, 20 microns, '
     print, 'and 1.7 nm respectively.'
     PRINT, '/Wiener_filter keyword will also apply said filter to the PRTF.'
     RETURN
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
           PRINT, '[ERROR] DM_PRTF: label_array must have as many entries ' $
                  + 'as filename_array!'
           return
        endif
     endelse
  endif
  
  ;; check/create eps filename
  filedir = FILE_DIRNAME(filename_array[0])
  filebase = FILE_BASENAME(filename_array[0],'.h5')
  path_sep = PATH_SEP()
  if (SIZE(eps_file,/type) ne 7) then begin
     if KEYWORD_SET(wiener_filter) then begin
        eps_file = STRJOIN([filedir,path_sep,filebase,'_wPRTF.eps'])
     endif else begin
        eps_file = STRJOIN([filedir,path_sep,filebase,'_PRTF.eps'])
     endelse
  endif

  ;; initialize arrays to store info from each file
  prtf_structs = PTRARR(n_files)
  q_inverse_um_array = PTRARR(n_files)

  ;; now populate the arrays
  for i=0,n_files -1 do begin
     if (status = DM_H5_OPENREAD(filename_array[i], $
                                 this_h5_id, error_string) ne 0) then begin
        PRINT, '[ERROR] DM_PRTF: '+error_string
        DM_PRTF_CLEANUP,n_files,prtf_structs,q_inverse_um_array
        return
     endif
     
     ;; adi array
     if (DM_H5_READ_ADI(this_h5_id, this_adi_struct, this_adi_array, $
                        this_adi_error_array, error_string) ne 0) then begin
        PRINT, '[ERROR] DM_PRTF: '+error_string
        DM_H5_CLOSE, this_h5_id
        DM_PRTF_CLEANUP,n_files,prtf_structs,q_inverse_um_array
        return
     endif
     this_cropped_struct = DM_DO_ADI_CROP(this_adi_struct, this_adi_array, $
                                         this_adi_error_array, $
                                         is_data_centered=is_data_centered)     
     this_adi_array = this_cropped_struct.adi_array

     ;; itn array
     if (DM_H5_READ_ITN(this_h5_id, this_itn_struct, this_itn_array, $
                        these_itn_errors, error_string) ne 0) then begin
        PRINT, '[ERROR] DM_PRTF: '+error_string
        DM_H5_CLOSE, this_h5_id
        DM_PRTF_CLEANUP,n_files,prtf_structs,q_inverse_um_array
        return
     endif
     
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
        PRINT, '[WARNING] DM_PRTF: ccd distance not found, using default'
        this_ccd_z_um = 128000.
     endelse
     if this_adi_struct.camera_x_pixelsize_meters gt 0.0 then begin
        this_ccd_pixelsize_um = this_adi_struct.camera_x_pixelsize_meters*1.e6
     endif else begin
        PRINT, '[WARNING] DM_PRTF: ccd pixel size not found, using default'
        this_ccd_pixelsize_um = 20. 
     endelse
     if this_adi_struct.lambda_meters gt 0.0 then begin
        this_wavelength_um = this_adi_struct.lambda_meters*1.e6
     endif else begin
        PRINT, '[WARNING] DM_PRTF: wavelength not found, using default'
        this_wavelength_um = 0.0016531361
     endelse
     
     ;; calculate PRTF
     this_prtf_struct = DM_DO_PRTF(this_adi_array,this_itn_array, $
                                  /is_real, is_data_centered=is_data_centered)
     
     this_q = FINDGEN(N_ELEMENTS(this_prtf_struct.prtf_array))

     q_inverse_um_array[i] = $
        PTR_NEW(this_q*this_ccd_pixelsize_um/ $
                (this_wavelength_um*this_ccd_z_um))

     ;; Wiener-filter
     if KEYWORD_SET(wiener_filter) then begin
        DM_PRTF_WIENER,this_itn_array,*(q_inverse_um_array[i]), $
                       filename_array[i], wiener_filter, $
                       step_size_pixels=step_size_pixels
        this_prtf_struct.prtf_array *= wiener_filter
     endif
     prtf_structs[i] = PTR_NEW(this_prtf_struct)
     dummy=WHERE(this_prtf_struct.prtf_array lt 0.5)
     ;print,dummy
     ;print,(*(q_inverse_um_array[i]))[dummy]

  endfor


  ;; determine maximum q range and minimum PRTF value
  q_max = 1.
  prtf_min = 1.
  for i=0, n_files-1 do begin
     this_max = MAX(*(q_inverse_um_array[i]))
     this_min = MIN((*prtf_structs[i]).prtf_array)
     if this_max gt q_max then $
        q_max = this_max
     if this_min lt prtf_min then $
        prtf_min = this_min
  endfor
  ;; add 10 to show a little more
  q_max += 10

  ;; calculate mtf curves
  n_mtfs = 0
  if N_ELEMENTS(mtf_resolutions_nm) GT 0 then begin
     n_mtfs = N_ELEMENTS(mtf_resolutions_nm)
     mtf_curves = PTRARR(n_mtfs) 
     mtf_freq = PTRARR(n_mtfs)
     mtf_label_array = STRARR(n_mtfs)
     nx = 512
     for i=0,n_mtfs-1 do begin
        this_mtf_curve = mtf(pupil(nx,nx, dr_nm=mtf_resolutions_nm[i]/1.22, $
                                   fx=this_freq_inverse_microns, stop=0))
        mtf_curves[i] = PTR_NEW(this_mtf_curve[nx/2.-1,nx/2.-1:*])
        mtf_freq[i] = PTR_NEW(this_freq_inverse_microns[nx/2.-1:*])
        mtf_label_array[i] = strtrim(STRING(ROUND(mtf_resolutions_nm[i])),2) $ 
                             +' nm'
     endfor
  endif

  ;; calculate label positions
  if not KEYWORD_SET(no_labels) then begin
     label_positions = FLTARR(n_files+n_mtfs)
     decrement = $
        ((0.69/(n_files+n_mtfs)) lt 0.1) ? (0.69/(n_files+n_mtfs)) : 0.1
     print, decrement
     for i=0,(n_files+n_mtfs) -1 do begin
        label_positions[i] = 0.84 - i*decrement
     endfor
  endif

  ;; create colors as indices into the color table 
  ;; that is loaded further down.
  n_colors = (n_files GE n_mtfs) ? n_files : n_mtfs
  colors = INTARR(n_colors)
  for i=0,n_colors-1 do begin
     colors[i] = 255 - i*(255/n_colors)
  endfor

  ;; plot in window
  WINDOW,/free, xsize=800,ysize=600
  DEVICE, decomposed = 0
  LOADCT, 39
  DM_PRTF_PLOT, n_files, q_inverse_um_array, prtf_structs, $
                q_max, prtf_min, colors, label_array, label_positions, $
                n_mtfs, mtf_curves, mtf_freq, mtf_label_array, $
                wiener_filter=wiener_filter

  ;; generate PS file
  old_plot = !D.name
  old_font = !P.font
  SET_PLOT, 'ps'
  DEVICE, file = eps_file, xsize = 6., ysize = 4., yoffset = 1., /inch, /color

  ;; this has been introduced in 7.1
  if (FLOAT(!version.release) ge 7.1) then $
     DEVICE, decomposed=0

  !P.font = 0

  ;; load color table Rainbow + black
  LOADCT, 40

  DM_PRTF_PLOT, n_files, q_inverse_um_array, prtf_structs, $
                q_max, prtf_min, colors, label_array, label_positions, $
                n_mtfs, mtf_curves, mtf_freq, mtf_label_array, $
                wiener_filter=wiener_filter

  DEVICE, /close
  PRINT, 'DM_PRTF: wrote eps file "'+eps_file+'"'
  SET_PLOT, old_plot
  !P.font = old_font  
  LOADCT, 0
  
  ;; free the pointer arrays
  DM_PRTF_CLEANUP, n_files, prtf_structs, q_inverse_um_array, $
                   n_mtfs, mtf_curves, mtf_freq

END
     

  
