x : num -> num =
	let a = 3 in
	let f = \(x : num) -> \(b : num) -> x + b
	in f(a);
