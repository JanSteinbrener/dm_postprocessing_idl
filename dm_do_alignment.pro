FUNCTION dm_do_alignment, reference_array, current_array

  ref_size = size(reference_array)
  cur_size = size(current_array)
  IF ref_size[1] NE cur_size[1] THEN BEGIN
     print, 'Arrays are not the same size'
     return
  ENDIF

END
