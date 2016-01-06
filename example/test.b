main : num = apply_n_times(5, \(n : num) -> n + 1, 1);

apply_n_times(n : num, f : num -> num, x : num) : num =
	if (n == 0)
		x
	else
		apply_n_times(n - 1, f, f(x));
