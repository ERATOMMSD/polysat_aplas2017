function M = rref_mathematica(A)
save('~/mathematica/input.dat', 'A', '-ascii');
system('~/mathematica/rref.mathematica');
N = load('~/mathematica/out_num.dat');
D = load('~/mathematica/out_den.dat');
M = sym(N)./sym(D);
end