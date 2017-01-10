#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

M = IntegerPart[Import["~/mathematica/input.dat"]];
R = RowReduce[M];
num = Numerator[R];
den = Denominator[R];
Export["~/mathematica/out_num.dat", num];
Export["~/mathematica/out_den.dat", den];