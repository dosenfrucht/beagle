main() : num = fact(5);

fact(n : num) : num = fact_(n, 1);

fact_(n : num, acc : num) : num = 
	if (n == 0)
		acc
	else
		fact_(n - 1, acc * n);


