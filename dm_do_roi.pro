FUNCTION dm_do_roi, array

  array_size = size(array)
  nx = array_size[1]
  ny = array_size[2]

  window, 1, xsize=1024, ysize=1024, $
          title='Define ROI by clicking top, bottom, left, and right'
  tvscl, congrid(abs(array), 1024, 1024)
  
  print, 'Click top, bottom, left, and right of ROI.'
  cursor, x1,y1, /dev
  WAIT, 0.5
  cursor, x2,y2, /dev
  WAIT, 0.5
  cursor, x3,y3, /dev
  WAIT, 0.5
  cursor, x4,y4, /dev

  x_points = [x1,x2,x3,x4]
  y_points = [y1,y2,y3,y4]
  max_x = max(x_points)
  min_x = min(x_points)
  max_y = max(y_points)
  min_y = min(y_points)

  dummy_array = fltarr(1024, 1024)
  dummy_array[min_x:max_x, min_y:max_y] = 1.0
  tvscl, dummy_array

  dummy_array = congrid(dummy_array, nx, ny)
  roi_index = where(dummy_array EQ 1.0)
  WDELETE, 1

  RETURN, roi_index

END
