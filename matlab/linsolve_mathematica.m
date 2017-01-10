function M = linsolve_mathematica(A, B)
A = double(A);
B = double(B);
save('~/mathematica/input.dat', 'A', '-ascii');
save('~/mathematica/input2.dat', 'B', '-ascii');
system('~/mathematica/linsolve.mathematica');
N = load('~/mathematica/out_num.dat');
D = load('~/mathematica/out_den.dat');
M = sym(N)./sym(D);
end