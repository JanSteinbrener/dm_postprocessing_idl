; MODIFICATION HISTORY:
;    2009-09-21 JFS: added Keywords verbose and
;                    get_ramps_from_1Dgaussian. Now will calculate and
;                    subtract ramps by averaging the first and last
;                    few data points. It will return the smaller one
;                    of both computed half periods.

FUNCTION dm_do_gauss2Dfit, array, x_range, y_range, pixel_size_nm, $
                           filename=filename, tilt=tilt, $
                           interpolation_points=interpolation_points, $
                           negative=negative, verbose=verbose, $
                           get_ramps_from_1Dgaussian=get_ramps_from_1Dgaussian

;;  Given the 2D array, the x and y ranges of the subarray, and pixel
;;  size the full array is interpolated and then the subarray is
;;  normalized between 0 and 1 and then fit to a Gaussian.  A contour
;;  plot is returned with the interpolated data and its fit, plus the
;;  FWHM line. If a filename is given an eps of the plot is also
;;  saved. Use keyword /tilt to allow rotation in Gaussian fit.

  IF (n_params() LT 4) THEN RETURN, 0  

  IF size(x_range, /n_elements) OR size(y_range, /n_elements) NE 2 THEN RETURN, 0

  IF NOT keyword_set(interpolation_points) THEN interpolation_points=20.

  array = abs(array)

  ;; add pixels on each side for interpolation
  interp_array = array[x_range[0]-2:x_range[1]+2, y_range[0]-2:y_range[1]+2]
  arr_size = size(interp_array)
  nx = arr_size[1]
  ny = arr_size[2]

  if KEYWORD_SET(verbose) then begin
     window, /f, title='Original array'
     tvscl, congrid(interp_array[2:nx-3,2:ny-3],200,200)
  endif

  ;; x and y arrays not including the added pixels
  xarr = DINDGEN((nx-4)*interpolation_points)/interpolation_points+2
  yarr = DINDGEN((ny-4)*interpolation_points)/interpolation_points+2
  interp_array = interpolate(interp_array, xarr, yarr, /grid, cubic=-0.5)  
  
  if KEYWORD_SET(verbose) then begin
     window, /f, title='Interpolated array'
     tvscl, congrid(interp_array, 200,200)
  endif

  arr_size = size(interp_array)
  nx = arr_size[1]
  ny = arr_size[2]

  ;; remove any linear ramps using 1D gaussian fit
  x_independent = DINDGEN(nx)/interpolation_points*pixel_size_nm
  y_independent = DINDGEN(ny)/interpolation_points*pixel_size_nm
 
  if KEYWORD_SET(get_ramps_from_1Dgaussian) then begin
     fit_x = gaussfit(x_independent, interp_array[*,ny/2], Ax, nterms=5)
     fit_y = gaussfit(y_independent, interp_array[nx/2,*], Ay, nterms=5)

     slope_x = Ax[4]
     offset_x = Ax[3]

     slope_y = Ay[4]
     offset_y = Ay[3]
  endif else begin
     ;; average first three and last three points and determine slope
     ;; from that
     xx1 = TOTAL(x_independent[0:2])/3
     xy1 = TOTAL(interp_array[0:2,ny/2])/3
     xx2 = TOTAL(x_independent[nx-3:*])/3
     xy2 = TOTAL(interp_array[nx-3:*,ny/2])/3
     slope_x = (xy2 - xy1)/(xx2 - xx1)
     offset_x = interp_array[0,ny/2]
     
     yx1 = TOTAL(y_independent[0:2])/3
     yy1 = TOTAL(interp_array[nx/2,0:2])/3
     yx2 = TOTAL(y_independent[ny-3:*])/3
     yy2 = TOTAL(interp_array[nx/2,ny-3:*])/3
     slope_y = (yy2 - yy1)/(yx2 - yx1)
     offset_y = interp_array[nx/2,0]
  endelse

  FOR i=0., nx-1 DO $
     interp_array[i,*] = interp_array[i,*] - (offset_x+slope_x*x_independent[i]) 
  FOR i=0., ny-1 DO $
     interp_array[*,i] = interp_array[*,i] - (offset_y+slope_y*y_independent[i])


  interp_array = interp_array - min(interp_array)
  interp_array = interp_array/max(interp_array)

  if KEYWORD_SET(verbose) then begin
     window, /f, title='Interpolated array with linear ramps removed'
     tvscl, congrid(interp_array, 200,200)
  endif

  IF keyword_set(filename) THEN BEGIN
     write_bin, filename+'.bin', interp_array
     write_tiff, filename+'.tif', interp_array/max(interp_array)*255.
     write_tiff, filename+'uninterp.tif', $
                 array[x_range[0]:x_range[1],y_range[0]:y_range[1]] / $
                 max(array[x_range[0]:x_range[1],y_range[0]:y_range[1]])*255.
     array[x_range[0]-1:x_range[1]+1, y_range[0]-1] = 0.
     array[x_range[0]-1:x_range[1]+1, y_range[1]+1] = 0.
     array[x_range[0]-1, y_range[0]-1:y_range[1]+1] = 0.
     array[x_range[1]+1, y_range[0]-1:y_range[1]+1] = 0.
     write_tiff, filename+'fullArray.tif', array/max(array)*255.
  ENDIF

  result = gauss2dfit(interp_array, A, negative=negative, tilt=tilt) 
;; A[2] = sigma_x,   A[3] = sigma_y
  FWHMx = 2.*sqrt(2.*alog(2.))*A[2]/interpolation_points*pixel_size_nm
  FWHMy = 2.*sqrt(2.*alog(2.))*A[3]/interpolation_points*pixel_size_nm

  if KEYWORD_SET(verbose) then begin
     print, 'major and minor axes full width at half maxima are ', FWHMx, FWHMy
     print, 'half-period widths are ', $
            FWHMx/(2.*sqrt(2.*alog(2.))), FWHMy/(2.*sqrt(2.*alog(2.)))
     print, A
  endif

  variable_x = DINDGEN(nx)
  variable_y = DINDGEN(ny)
  z = DBLARR(nx,ny)

  half_period = MIN([FWHMx, FWHMy])/(2.*sqrt(2.*alog(2.)))

  for x=0, nx-1 DO BEGIN
     for y=0, ny-1 DO BEGIN
        z[x,y]=(((variable_x[x]-A[4])*cos(A[6])-(variable_y[y]-A[5])*sin(A[6])) $
                /A[2])^2 + (((variable_x[x]-A[4])*sin(A[6])-$
                             (variable_y[y]-A[5])*cos(A[6]))/A[3])^2
     endfor
  endfor
  gaussian = A[0]+A[1]*exp((-z)/2.)

  x_axis = FINDGEN(nx)/interpolation_points*pixel_size_nm
  y_axis = FINDGEN(ny)/interpolation_points*pixel_size_nm

;; contour plots
  spacing=0.22 ;how often contour lines are plotted
  levels = FINDGEN(FLOOR((max(interp_array)-min(interp_array))/spacing))
  ;labels = levels
  remainder = min(interp_array) mod spacing
  FOR i=0, FLOOR((max(interp_array)-min(interp_array))/spacing)-1 DO BEGIN
     levels[i]=(min(interp_array)+(spacing-remainder))+i*spacing
     ;labels[i] = 0.
     ;if levels[i] EQ 0.5 then labels[i]=1.
  ENDFOR
  
  if KEYWORD_SET(verbose) then begin
     ;; 1 = red, 5 = orange,3 = green
     tvlct, [255,255,0],[0,150,255],[0,0,0,0],1
     device, decompose=0
     window, xsize=600, ysize=600, /f
     contour, interp_array,x_axis, y_axis, c_colors=[1,2,3], c_charsize=1.5, $
              xstyle=1, ystyle=1, xtitle='width [nm]', ytitle='width [nm]', $
              levels=levels, c_labels=0
     contour, gaussian, x_axis, y_axis, c_color=[1,2,3], xstyle=4, ystyle=4,$
              /noerase, c_charsize=1.5, levels=levels,c_labels=0,c_linestyle=2
     
     print, 'Half max is ',max(gaussian)/2.
     
     ;; plot FWHM
     t = FINDGEN(360)/359.*2*!pi
     x = A[4]/interpolation_points*pixel_size_nm + FWHMx/2.*cos(t)*cos(A[6]) - $
         FWHMy/2.*sin(t)*sin(A[6])
     y = A[5]/interpolation_points*pixel_size_nm + FWHMx/2.*cos(t)*sin(A[6]) - $
         FWHMy/2.*sin(t)*cos(A[6])
     oplot,x,y
  endif

  IF keyword_set(filename) THEN BEGIN
;; save pre-existing graphics devices and font
     old_plot = !d.name
     old_font = !p.font
;; go to PostScript and use native PostScript fonts (!p.font =0)
     set_plot, 'ps'
     !p.font = 0

     device, file = filename+'.eps', /encap, /inch, decomposed=0, color=1
     contour, interp_array,x_axis, y_axis, c_colors=[1,2,3], c_charsize=1.5, $
              xstyle=1, ystyle=1, xtitle='width [nm]', ytitle='width [nm]', $
              levels=levels, c_labels=0
     contour, gaussian, x_axis, y_axis, c_color=[1,2,3], xstyle=4, ystyle=4,$
              /noerase,c_charsize=1.5,levels=levels,c_labels=0,c_linestyle=2
     oplot, x, y
     
     device, /close
     print, 'Wrote file "'+filename+'.eps"'
     
;; go back to pre-existing graphics mode and font
     set_plot, old_plot
  ENDIF

  RETURN,half_period
END
