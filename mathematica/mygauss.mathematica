#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

A = IntegerPart[Import["~/mathematica/input.dat"]];
B = RowReduce[A];
rows = Length[A];
columns = Length[Transpose[A]];
U = IdentityMatrix[columns];
S = ConstantArray[0, {columns, columns}];
prev = 0;
tail = columns;
For[i=1, i <= rows, i++, 
	 finding = Position[B[[i]], 1];
	 If[finding != {},
	  	 index = finding[[1,1]];
	 	  U[[index]] = B[[i]];
		  S[[index,i]] = 1;
		  For[j=prev+1, j <= index-1, j++,
		  		S[[j, tail]] = 1;
				tail = tail - 1;
		  ];
		  prev = index;
	];
];
For[j=prev+1, j <= columns, j++,
	      S[[j, tail]] = 1;
	      tail = tail - 1;
];

Uinv = U;
U = U*(-1);
U = U + IdentityMatrix[columns]*2;
U = U.S;
Uinv = Transpose[S].Uinv;
num = Numerator[B];
den = Denominator[B];
Export["~/mathematica/B_num.dat", num];
Export["~/mathematica/B_den.dat", den];
num = Numerator[U];
den = Denominator[U];
Export["~/mathematica/U_num.dat", num];
Export["~/mathematica/U_den.dat", den];
num = Numerator[Uinv];
den = Denominator[Uinv];
Export["~/mathematica/Uinv_num.dat", num];
Export["~/mathematica/Uinv_den.dat", den];