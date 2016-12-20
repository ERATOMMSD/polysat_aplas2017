% add m-multiplicated i-th row to j-th row
function A = addrow(n, m, i, j)
    A = eye(n);
    A(j,i) = m;
end