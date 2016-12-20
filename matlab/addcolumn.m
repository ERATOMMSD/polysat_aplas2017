% add m-multiplicated i-th column to j-th column
function A = addcolumn(n, m, i, j)
    A = eye(n);
    A(i,j) = m;
end