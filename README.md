PolySAT
====

An implementation of interpolant synthesize by [Liyun Dai, et al. CAV 2013][^1]
and a bit more features.

[^1]: Liyun Dai, Bican Xia, and Naijun Zhan. In the Proc.\ of CAV, pp.364--380,
    2013.

## Requirements

- [OCaml](https://ocaml.org) (compiler)
- omake (build tool)
- ppx_deriving (ocaml library)
- sexplib (ocaml library)
- ounit (ocmal library if you need to run unit tests)

All the tool and libraries can be installed easily by
[OPAM](https://opam.ocaml.org) an OCaml package manager.

- [YALMIP](http://users.isy.liu.se/johanl/yalmip/) (tools for MATLAB which does
  not required to run this tool but required to run generated MATLAB code)

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

``` scheme:example/circles.smt
(interpolant
 (>= 1 (+ (* x x) (* y y)))
 (>= 1 (+ (* x x) (* (- y 4) (- y 4))))
 (+ (* ? y) ?)
 2)
```

The command `interpolant` synthesizes an interpolant between the first argument
and the second argument.  So the file above insists to synthesize an interpolant
polynomial inequality `P(x,y) > 0` implied by `(>= 1 (+ (* x x) (* y y)))` and
contradicted by `(>= 1 (+ (* x x) (* (- y 4) (- y 4))))`.

The third argument is the shape of the polynomial `P(x,y)` desired.  The
argument is not required if there is no need to restrict the shape.

The fourth argument (or third argument if shape is not given) is the degree of
polynomials the tool searchs.  Given higher degree, the tool find more
interpolants (so you should increase the degree if no interpolant finds), but a
synthesized interpolant tend to become complex and ofcourse calculation cost
increases.

The main program takes a problem file and print out MATLAB code synthesizing an
interpolant of the problem.

```
$ ./polysat example/circles.smt > circles.m
```

Running the code in MATLAB, the following output will be generated.

```
Interior-point solution summary
  Problem status  : PRIMAL_AND_DUAL_FEASIBLE
  Solution status : OPTIMAL
  Primal.  obj: -3.4999999463e+00   Viol.  con: 3e-08    var: 0e+00    barvar: 0e+00
  Dual.    obj: -3.4999999595e+00   Viol.  con: 0e+00    var: 2e-09    barvar: 9e-10
Optimizer summary
  Optimizer                 -                        time: 0.62
    Interior-point          - iterations : 13        time: 0.41
      Basis identification  -                        time: 0.00
        Primal              - iterations : 0         time: 0.00
        Dual                - iterations : 0         time: 0.00
        Clean primal        - iterations : 0         time: 0.00
        Clean dual          - iterations : 0         time: 0.00
        Clean primal-dual   - iterations : 0         time: 0.00
    Simplex                 -                        time: 0.00
      Primal simplex        - iterations : 0         time: 0.00
      Dual simplex          - iterations : 0         time: 0.00
      Primal-dual simplex   - iterations : 0         time: 0.00
    Mixed integer           - relaxations: 0         time: 0.00

2.499999978-4.8890e-11*x-1.0000*y-1.4528e-10*y^2-3.6292e-10*y^3-6.7853e-11*x^2-3.8967e-11*x^3-1.6150e-10*x^4-1.2670e-10*y^4-4.7616e-11*x*y-3.8967e-11*x*y^2-3.9523e-11*x*y^3-3.6292e-10*x^2*y-2.8820e-10*x^2*y^2-3.9523e-11*x^3*y
```

The last line is the interpolant `P(x,y)` if `Problem status` in the second line
is `PRIMAL_AND_DUAL_FEASIBLE`.

## Team

- Kohei Suenaga
- Yuki Nishida
- Akifumi Imanishi
- Minoru Kinoshita

