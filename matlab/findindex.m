function n = findindex(A)
    f = find(A, 1);
    if(size(f, 1) == 0 | size(f, 2) == 0)
        n = 0;
    else
        n = f;
    end
end