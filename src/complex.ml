class complex (real:float) (imaginary:float) =
  object
    val _real_part = real
    val _imaginary_part = imaginary

    method get_real_part = _real_part
    method get_imaginary_part = _imaginary_part

    method expand = (_real_part, _imaginary_part)

    method describe =
      if (_real_part <> 0.) then
        "(" ^ (string_of_float _real_part) ^ " + " ^ (string_of_float _imaginary_part) ^ "i)"
      else
        (string_of_float _imaginary_part) ^ "i"
  end
