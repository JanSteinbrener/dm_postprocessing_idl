;+
; NAME:
;    DM_DO_PLANEFIT
;
; PURPOSE:
;
;    The program calls performs a least square fit of z = a*x + b*y + c
;    on the itn_array to calculate the shift needed to center the
;    adi_array. The ROI which the user defines should be within
;    the object's support. This routine was rewritten from CJ's remove_plane.c
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
;    coeffients = dm_do_planefit(itn_array)
;
;
; RETURN VALUE:
;
;    coeffients = [a, b, c]
;
;
; INPUT PARAMETERS:
;
;    itn_array   
;
;
; OPTIONAL INPUT:
;
;    none
;
;
; INPUT KEYWORDS:
;
;    none
;
; MODIFICATION HISTORY:
;    2009-01-19 JN: written
;-

FUNCTION dm_do_planefit, itn_array

  array_size = size(itn_array)
  nx = array_size[1]
  ny = array_size[2]
  
;; define ROI by clicking top, bottom, left and right (in any order)
  window, 1, xsize=nx, ysize=ny, $
          title='Left click top, bottom, left, and right edges of ROI.'
  tvscl, congrid(abs(itn_array),nx,ny)
  print, 'Click top, bottom, left, and right edges of ROI'
  cursor, x1,y1, /dev
  WAIT, 0.5
  cursor, x2,y2, /dev
  WAIT, 0.5
  cursor, x3,y3, /dev
  WAIT, 0.5
  cursor, x4,y4, /dev

   x_points = [x1,x2,x3,x4]
   y_points = [y1,y2,y3,y4]
   x_min = min(x_points)
   x_max = max(x_points)
   y_min = min(y_points)
   y_max = max(y_points)
   roi = itn_array[x_min:x_max, y_min:y_max]

   roi_size = size(roi)
   roi_nx = roi_size[1]
   roi_ny = roi_size[2]
   roi_n = roi_size[4]

   wdelete, 1
   window, 1, xsize=nx/2, ysize=ny/2
   tvscl, congrid(abs(roi),nx/2, ny/2)
;; initiallize variables
   sum_x = double(0.)
   sum_y = double(0.)
   sum_z = double(0.)
   sum_x2 = double(0.)
   sum_y2 = double(0.)
   sum_z2 = double(0.)
   sum_xy = double(0.)
   sum_xz = double(0.)
   sum_yz = double(0.)

   FOR y = 0, roi_ny-1 DO BEGIN
      FOR x = 0, roi_nx-1 DO BEGIN
         sum_x = sum_x + x
         sum_y = sum_y + y
         sum_z = sum_z + roi[x,y]
         sum_x2 = sum_x2 + x^2
         sum_y2 = sum_y2 + y^2
         sum_z2 = sum_z2 + roi[x,y]*roi[x,y]
         sum_xy = sum_xy + x*y
         sum_xz = sum_xz + x*roi[x,y]
         sum_yz = sum_yz + y*roi[x,y]
      ENDFOR
   ENDFOR
   
   a_numerator = sum_xz*(roi_n*sum_y2 - sum_y*sum_y)-$
                 sum_xy*(roi_n*sum_yz - sum_y*sum_z)+$
                 sum_x*(sum_yz*sum_y - sum_y2*sum_z)
   a_denominator = sum_x2*(roi_n*sum_y2 - sum_y*sum_y)-$
                   sum_xy*(roi_n*sum_xy - sum_x*sum_y)+$
                   sum_x*(sum_xy*sum_y - sum_y2*sum_x)
   IF a_denominator NE 0.0  THEN a = a_numerator/a_denominator $
   ELSE a = 0.0

   b_numerator = sum_x2*(roi_n*sum_yz - sum_y*sum_z)-$
                 sum_xz*(roi_n*sum_xy - sum_x*sum_y)+$
                 sum_x*(sum_xy*sum_z - sum_yz*sum_x)
   b_denominator = a_denominator
   IF b_denominator NE 0.0 THEN b = b_numerator/b_denominator $
   ELSE b = 0.0
   
   c_numerator = sum_x2*(sum_y2*sum_z - sum_y*sum_yz)-$
              sum_xy*(sum_xy*sum_z - sum_x*sum_yz)+$
                 sum_xz*(sum_xy*sum_y - sum_x*sum_y2)
   c_denominator = a_denominator
   IF c_denominator NE 0.0 THEN c = c_numerator/c_denominator $
   ELSE c = 0.0
 
   coeffients = [a,b,c]
   RETURN, coeffients

END
