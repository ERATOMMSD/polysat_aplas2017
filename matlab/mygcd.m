function n = mygcd(X)
	if length(X) == 0
		n = 0;
	elseif length(X) == 1
		n = X(1);
	elseif length(X) == 2
		n = gcd(X(1), X(2));
	else
		Y = X(2:length(X));
		Y(1) = gcd(X(1), X(2));
		n = mygcd(Y);
	end
end

