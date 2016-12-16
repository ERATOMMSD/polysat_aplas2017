function b = check_psd(X)
	[m,n] = size(X);
	if m ~= n
		b = false;
	else
		b = true;
		for i = 1:n
			if det(X(1:i,1:i)) < 0
				b = false;
				break;
			end
		end
	end
end
