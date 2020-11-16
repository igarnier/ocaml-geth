let bits width z = Array.init width (fun i -> Z.testbit z (width - i - 1))
