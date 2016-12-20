% swap i-th row and j-th row
function A = swaprow(n, i, j)
    if i == j
        A = eye(n);
    else
        A = eye(n);
        A(i,i) = 0;
        A(i,j) = 1;
        A(j,i) = 1;
        A(j,j) = 0;
    end
end