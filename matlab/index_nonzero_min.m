function n = index_nonzero_min(A)
    n = 0;
    now = inf;
    for i = 1:length(A)
        if (A(i) ~= 0) && (A(i) < now)
            n = i;
            now = A(i);
        end
    end
end