function [B,U,Uinv] = mygauss_mathematica(A)
A = double(A);
save('~/mathematica/input.dat', 'A', '-ascii');
system('~/mathematica/mygauss.mathematica');
Bnum = load('~/mathematica/B_num.dat');
Bden = load('~/mathematica/B_den.dat');
Unum = load('~/mathematica/U_num.dat');
Uden = load('~/mathematica/U_den.dat');
Uinvnum = load('~/mathematica/Uinv_num.dat');
Uinvden = load('~/mathematica/Uinv_den.dat');
B = sym(Bnum)./sym(Bden);
U = sym(Unum)./sym(Uden);
Uinv = sym(Uinvnum)./sym(Uinvden);
end