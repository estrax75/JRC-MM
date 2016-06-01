FUNCTION cgi_map_bitwise_flag, statusmap, bitposition

  return, BYTE((statusmap AND (2UL^bitposition))/(2UL^bitposition))

END