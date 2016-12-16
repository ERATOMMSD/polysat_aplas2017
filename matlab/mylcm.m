function n = mylcm(X)
	if length(X) == 0
		n = 0;
	elseif length(X) == 1
		n = X(1);
	elseif length(X) == 2
		n = lcm(X(1), X(2));
	else
		Y = X(2:length(X));
		Y(1) = lcm(X(1), X(2));
		n = mylcm(Y);
	end
end

