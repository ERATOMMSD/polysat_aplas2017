#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

A = IntegerPart[Import["~/mathematica/input.dat"]];
B = IntegerPart[Import["~/mathematica/input2.dat"]];
R = LinearSolve[A, B, Method -> "OneStepRowReduction"];
num = Numerator[R];
den = Denominator[R];
Export["~/mathematica/out_num.dat", num];
Export["~/mathematica/out_den.dat", den];