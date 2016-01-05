apply(f : num -> num, x : num) : num = f(x);

main : num = 11 + 12;

apply_n_times(n : num, f : num -> num, x : num) : num =
	if (n == 0)
		x
	else
		apply_n_times(n - 1, f, f(x));
