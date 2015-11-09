main() : num = {
	show_num(1337);
	put_string_line("");
};

fact(n : num) : num = fact_(n, 1);

fact_(n : num, acc : num) : num = {
	put_string_line("Hallo Daniel");
	if (n == 0)
		acc
	else
		fact_(n - 1, acc * n);
};


show_num(n : num) : num =
	if (n == 0)
		put_string("0")
	else
		show_num_(n);

show_num_(n : num) : num =
	if (n == 0)
		0
	else {
		show_num_(n / 10);
		put_string(digit_to_string(n % 10));
	};


digit_to_string(n : num) : str =
	if (n == 0)
		"0"
	else if (n == 1)
		"1"
	else if (n == 2)
		"2"
	else if (n == 3)
		"3"
	else if (n == 4)
		"4"
	else if (n == 5)
		"5"
	else if (n == 6)
		"6"
	else if (n == 7)
		"7"
	else if (n == 8)
		"8"
	else
		"9";
