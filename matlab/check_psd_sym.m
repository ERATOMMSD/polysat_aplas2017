

function b = check_psd_sym(X)
	[m,n] = size(X);
	if m ~= n
		b = false;
	else
		b = true;
		for i = 1:n
            choices = nchoosek([1:n], i);
            for j = 1:size(choices, 1)
                if isAlways(det(X(choices(j), choices(j))) < 0)
                    b = false;
                    break;
                end
            end
		end
	end
end
