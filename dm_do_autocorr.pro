;+
; NAME:
;    DM_DO_AUTOCORRELATION
;
; PURPOSE:
;
;    This function applies a Gaussian function to the given adi_array
;    to suppress the missing center pixels then performs an
;    autocorrelation of the real space image.
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
;    autocorr = dm_do_autocorr(adi_array [,sigma=sigma, /complex])
;
; RETURN VALUE:
;
;    autocorr:  autocorrelation of adi_array after a Gaussian filter
;
;
; INPUT PARAMETERS:
;
;    adi_array: This is assumed to be fft-centered. Can be 3D or 2D.
;
;
; INPUT KEYWORDS:
;
;    sigma:  Default is 0.01 if not given.
;
;    complex:  Set keyword to return a complex
;        autocorrelation. Default is the normalized magnitude of the 
;        autocorrelation.
;
;    is_data_centered: Set this keyword to indicate that the incoming
;        array is already data-centered. Otherwise it is assumed to be
;        fft-centered and will be shifted accordingly.
;
; MODIFICATION HISTORY:
;    2009-01-15 JN: written
;    2009-01-19 JN: added keyword /complex
;    2009-02-12 JFS: added keyword /is_data_centered
;    2009-06-24 JFS: 3D-fied function. 
;-

FUNCTION dm_do_autocorr, adi_array, sigma=sigma, complex=complex, $
                         is_data_centered=is_data_centered

  IF NOT keyword_set(threshold) THEN threshold = 0.01

  adi_size = size(adi_array,/dimensions)

  ;; shift to data-centered
  if not KEYWORD_SET(is_data_centered) then begin
     if N_ELEMENTS(adi_size) eq 2 then begin 
        adi_array = SHIFT(adi_array,adi_size[0]/2,adi_size[1]/2) 
     endif else begin
        adi_array = SHIFT(adi_array,adi_size[0]/2,adi_size[1]/2,adi_size[2]/2)
     endelse
  endif

;; Remove any negatives from intensities
  neg = where(adi_array LT 0, count)
  IF count NE 0 THEN adi_array[neg] = 0.0 

;; take autocorrelation after Gaussian filter
  if N_ELEMENTS(adi_size) eq 2 then begin
     radius = shift(dist(adi_size[0],adi_size[1]),adi_size[0]/2,adi_size[1]/2)
  endif else begin
     radius = FLTARR(adi_size)
     for i=0L,adi_size[0]-1 do begin
        for j=0L,adi_size[1]-1 do begin
           for k=0L,adi_size[2]-1 do begin
              radius[i,j,k] = SQRT((ABS(i-adi_size[0]/2))^2 + $
                                   (ABS(j-adi_size[1]/2))^2 + $
                                   (ABS(k-adi_size[2]/2))^2)
           endfor
        endfor
     endfor
  endelse

  IF n_elements(sigma) EQ 0 THEN sigma=0.15
  width = adi_size[0]*sigma
  filter = (radius/width)^4 * exp(2-2*(radius/width)^2)

  width_index = where(radius GE width, count)
  IF count NE 0 THEN filter[width_index] = 1.0  ;; outside width filter is 1
  
  autocorr = bh_sfft(filter*adi_array,-1)
  IF NOT keyword_set(complex) THEN BEGIN
     autocorr = abs(autocorr)
     autocorr = autocorr/max(autocorr)
  ENDIF

  ;; shift back to fft-centered
  if not KEYWORD_SET(is_data_centered) then begin
     if N_ELEMENTS(adi_size) eq 2 then begin
        adi_array = SHIFT(adi_array,adi_size[0]/2,adi_size[1]/2)
     endif else begin
        adi_array = SHIFT(adi_array,adi_size[0]/2,adi_size[1]/2,adi_size[2]/2)
     endelse
  endif

  RETURN, autocorr
END
