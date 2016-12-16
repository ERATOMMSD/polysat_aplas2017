function Y = approximate(X, eps)
	Z = X./max(abs(X));
	[A,B] = rat(Z', eps);
	Y = A.*mylcm(B)./B;
	Y = Y./mygcd(Y);
end
