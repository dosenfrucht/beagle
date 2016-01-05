fact(n : num) : num =
	if (n == 0)
		1
	else
		n * fact(n - 1);

main : num = fact(4);
