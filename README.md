PolySAT
====
An implementation of interpolant generation by [1].  We understand that the following workflow is too troublesome, so we are preparing a refactored version that does not depend on MATLAB now.

[1] Takamasa Okudono, Yuki Nishida, Kensuke Kojima, Kohei Suenaga, Kengo Kido and Ichiro Hasuo.  In the Proc. of APLAS, pp.491-533, 2017.

## Requirements
### To compile and run this program
- [OCaml](https://ocaml.org) (compiler)
- omake (build tool)
- ppx_deriving (OCaml library)
- sexplib (OCaml library)
- ounit (OCaml library if you need to run unit tests)

All the tool and libraries can be installed easily by
[OPAM](https://opam.ocaml.org) an OCaml package manager.

### To run codes generated by this program
- MATLAB R2016b
- Symbolic Math Toolbox for MATLAB
- [YALMIP](http://users.isy.liu.se/johanl/yalmip/) (follow [the installation guide](https://yalmip.github.io/tutorial/installation/))
- SDP solver that YALMIP supports.  For example:
  - [SDPT3](http://www.math.nus.edu.sg/%7Emattohkc/sdpt3.html) (follow the instruction of the website)
  - [SeDuMi](https://github.com/sqlp/sedumi/)

And you have to modify Symbolic MathToolbox by adding 
```elseif isa(p, 'sym')
        symb_p = char(p);
```
to `extras/sdisplay.m`.

## Compile

### Main program

``` shell
$ omake
```

will generates the program `polysat` in the project root directory.

### Internal code documents

``` shell
$ omake doc
```

will generates the HTML document in the directory `src/polysat.doc/`.

### Run unit tests

```
$ omake test
```

will run the unit tests.

## Usage

This tool synthesize an interpolant of the given problem.  Contents of problem
file is like follows.

``` scheme:example/simple_circle.smt
(interpolant
 (<= y (- 1))
 (< (+ (* x x) (* y y)) 1)
 1)
```

The command `interpolant` synthesizes an interpolant between the first argument
and the second argument.  So the file above insists to synthesize an interpolant
polynomial inequality `P(x,y) > 0` implied by `(<= y (- 1))` and
contradicted by `(< (+ (* x x) (* y y)) 1)`.

The third argument is the degree of polynomials the tool searches (in the experiment section, it is represented by `b`).  Given higher degree, the tool find more
interpolants (so try the increment of the degree if no interpolant finds), but a
synthesized interpolant tend to become complex and ofcourse calculation cost
increases.

The main program takes a problem file and print out MATLAB code synthesizing an
interpolant of the problem.  To change the precision (in the experiment section, it is represented by `c`), change the first line of the generated code.


```
$ ./polysat example/simple_circle.smt > simple_circle.m
```

Running the code in MATLAB, the following output will be generated.

```

>> simple_circle
SeDuMi 1.3 by AdvOL, 2005-2008 and Jos F. Sturm, 1998-2003.
Alg = 2: xz-corrector, theta = 0.250, beta = 0.500
Put 20 free variables in a quadratic cone
eqs m = 25, order n = 17, dim = 60, blocks = 6
nnz(A) = 78 + 0, nnz(ADA) = 625, nnz(L) = 325
 it :     b*y       gap    delta  rate   t/tP*  t/tD*   feas cg cg  prec
  0 :            1.88E+00 0.000
  1 :   0.00E+00 5.75E-01 0.000 0.3055 0.9000 0.9000   0.45  1  1  3.0E+00
  2 :   0.00E+00 1.28E-01 0.000 0.2229 0.9000 0.9000   0.15  1  1  1.5E+00
  3 :   0.00E+00 4.26E-03 0.000 0.0332 0.9900 0.9900  -0.70  1  1  1.1E+00
  4 :   0.00E+00 1.54E-04 0.000 0.0362 0.9900 0.9900  -0.99  1  1  1.1E+00
  5 :   0.00E+00 4.22E-05 0.000 0.2734 0.9000 0.9000  -1.00  1  1  1.2E+00
  6 :   0.00E+00 2.60E-06 0.000 0.0617 0.9900 0.9900  -1.01  1  1  2.1E+00
  7 :   0.00E+00 1.01E-07 0.000 0.0390 0.9900 0.9900  -1.01  5  5  2.2E+00
  8 :   0.00E+00 2.70E-08 0.000 0.2663 0.9000 0.9000  -1.01  7  6  2.1E+00
  9 :   0.00E+00 1.56E-09 0.000 0.0580 0.9900 0.9900  -1.01  6  7  1.7E+00
 10 :   0.00E+00 7.39E-11 0.298 0.0472 0.9900 0.9900  -1.00  8  8  1.7E+00

Dual infeasible, primal improving direction found.
iter seconds  |Ax|    [Ay]_+     |x|       |y|
 10      0.4   3.3e-10   6.2e-11   1.3e+01   3.2e+00

Detailed timing (sec)
   Pre          IPM          Post
1.137E-01    4.043E-01    2.472E-02    
Max-norms: ||b||=0, ||c|| = 1,
Cholesky |add|=0, |skip| = 2, ||L.L|| = 20512.9.
Infeasible
SeDuMi 1.3 by AdvOL, 2005-2008 and Jos F. Sturm, 1998-2003.
Alg = 2: xz-corrector, theta = 0.250, beta = 0.500
Put 25 free variables in a quadratic cone
eqs m = 27, order n = 19, dim = 67, blocks = 6
nnz(A) = 101 + 0, nnz(ADA) = 729, nnz(L) = 378
 it :     b*y       gap    delta  rate   t/tP*  t/tD*   feas cg cg  prec
  0 :            1.68E+00 0.000
  1 :   0.00E+00 5.58E-01 0.000 0.3314 0.9000 0.9000   0.93  1  1  2.5E+00
  2 :   0.00E+00 1.16E-01 0.000 0.2085 0.9000 0.9000   0.93  1  1  5.4E-01
  3 :   0.00E+00 7.76E-03 0.011 0.0667 0.9900 0.9900   0.86  1  1  3.9E-02
  4 :   0.00E+00 7.62E-04 0.000 0.0981 0.9900 0.9900   0.92  1  1  4.0E-03
  5 :   0.00E+00 3.53E-05 0.000 0.0464 0.9900 0.9900   0.99  1  1  1.9E-04
  6 :   0.00E+00 1.00E-05 0.000 0.2838 0.9000 0.9000   0.86  2  1  5.8E-05
  7 :   0.00E+00 1.92E-06 0.000 0.1911 0.9000 0.9000   0.88  2  2  1.2E-05
  8 :   0.00E+00 8.68E-08 0.357 0.0453 0.9900 0.9900   0.94  6  6  5.5E-07
  9 :   0.00E+00 3.03E-09 0.000 0.0349 0.9900 0.9900   0.97  7 11  2.0E-08
 10 :   0.00E+00 3.04E-11 0.201 0.0101 0.9990 0.9990   0.99 13 13  2.0E-10

iter seconds digits       c*x               b*y
 10      0.1  11.3 -3.9433535028e-11  0.0000000000e+00
|Ax-b| =   1.4e-10, [Ay-c]_+ =   4.6E-11, |x|=  8.7e+00, |y|=  8.1e+00

Detailed timing (sec)
   Pre          IPM          Post
9.807E-03    6.716E-02    3.088E-03    
Max-norms: ||b||=0, ||c|| = 1,
Cholesky |add|=0, |skip| = 3, ||L.L|| = 9744.07.
Checking semidefiniteness...
Checking strictcone condition...
Checking equality...
Cannot certify the validity of this interpolant.
Checking semidefiniteness...
Checking strictcone condition...
Checking equality...
Cannot certify the validity of this interpolant.
Checking semidefiniteness...
Checking strictcone condition...
Checking equality...
interpolant := 33*y^2 - 70*y - 103 >= 0
This interpolant is valid.
Elapsed time is 3.710231 seconds.
>> 
```

If a line starts from `interpolant := ` appeared, then the solver succeeded in the generation of an interpolant (in this case, it is `33*y^2 - 70*y - 103 >= 0`).

## Troubleshooting
### Runtime error occurs at `sdisplay`
Please read "To run codes generated by this program" section and modify MATLAB Symbolic Math Toolbox.

### No interpolants are generated / generated interpolants are dirty
- Numerical solutions of the SDP problem are affected by the environment (machines or solvers).  Trying another SDP solver is sometimes effective.
- Please try another setting by changing the maximum degree `b` or the precision `c`

## Team

- Kohei Suenaga
- Yuki Nishida
- Akifumi Imanishi
- Minoru Kinoshita
- Takamasa Okudono
- Kengo Kido
