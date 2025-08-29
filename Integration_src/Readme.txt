Archive content:
 - Integration.pas
   Definite integrals related Free Pascal unit, defining the following two functions:
    - Integrate(): Numerical integration using the Adaptive Simpson's method of a mathematical function passed as parameter.
	- IntegralArea(): Plot of the graph with the integral area of a function passed as parameter.
 - integration1.*: Simple Lazarus command line project, showing how to use the Integrate() function.
 - integration2.*: Simple Lazarus command line project, showing how to use the IntegralArea() function.
 
Note:
 IntegralArea() calls the freeware program "gnuplot" to generate the plot (as PNG), thus, this software must be installed to use the function!
