;+
; NAME:
;    DM_DO_SHRINKWRAP
;
; PURPOSE:
;
;    This program applies the shrinkwrap method (Marchesini, 2003) to
;    update the support for a reconstruction algorthim.  Modified from
;    find_support.pro from Xiaojing Huang.
;    Sigma and threshold values will vary from data set to data set,
;    but a good starting point is sigma=3, thresholdd=0.06.
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
;    support = dm_do_shrinkwrap(current_image [,sigma, threshold])
;
; RETURN VALUE:
;
;    support:  New support found from shrinkwrap.
;
;
; INPUT PARAMETERS:
;
;    current_image:  Current reconstruction estimate to apply shrinkwrap.
;
;
; OPTIONAL INPUT/OUTPUT PARAMETERS:
;
;    SIGMA:  Defines width of Gaussian blur.  If undefined then 
;        uses default of 3.
;
;    THRESHOLD:  Defines threshold applied to blurred image to define 
;        new mask.  If undefined then uses default of 0.06.
;
;
; MODIFICATION HISTORY:
;    2008-07-30 JN: Modified from find_support.pro
;    2008-10-31 JS: Added Keyword highpass to highpass filter before
;                   applying Gaussian blur
;    2009-01-16 JN: Renamed to conform with nomenclature.
;                   Removed highpass since it now is its own function.
;                   Changed sigma and threshold to optional
;                   parameters.
;    2009-11-24 JFS: 3dfied.
;-

FUNCTION dm_do_shrinkwrap, current_image, sigma, threshold

  IF (n_elements(sigma) EQ 0) THEN sigma = 3

  IF (n_elements(threshold) EQ 0) THEN threshold = 0.06
  
  dims = SIZE(current_image,/dimensions)

  magnitude = abs(current_image)
  range = round(3*sigma) ;; 2.3548*sigma is FWHM
 
  ;; take autocorrelation after Gaussian filter
  if N_ELEMENTS(dims) eq 2 then begin
     r = shift(dist([range*2+1,range*2+1]),range,range)
  endif else begin
     r = FLTARR([range*2+1,range*2+1,range*2+1])
     for i=0L,range*2+1-1 do begin
        for j=0L,range*2+1-1 do begin
           for k=0L,range*2+1-1 do begin
              r[i,j,k] = SQRT((ABS(i-(range*2+1)/2))^2 + $
                              (ABS(j-(range*2+1)/2))^2 + $
                              (ABS(k-(range*2+1)/2))^2)
           endfor
        endfor
     endfor
  endelse
  

  gaussian = exp(-(r/sigma)^2/2)

  gaussian_norm = gaussian/total(gaussian)

  convolution = convol(magnitude, gaussian_norm, /center)
  maximum = max(convolution)

;;define new support to be inside threshold
  support = (convolution GT maximum*threshold) 

  RETURN, support
END
