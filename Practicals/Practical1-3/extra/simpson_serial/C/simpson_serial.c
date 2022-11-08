/*
* Calculate the area under the curve using Simpson's rule
* Compile: 
*	$gcc simpson_serial.c -o simpson_serialc 
* Run:
* 	$./simpson_serialc 
* Ex: a=0 b=90 n=100 (n: even)
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//The syntax for the sin function in the C language is:double sin(double x)
//x is value expressed in radians (not degrees).
double degtorad(double degang) {
   return( (M_PI * degang)/180.0 );
}

double func(double x){
  return sin(x)*sin(x);
}

//Simpson's function
double simpson(int n, double h, double *fx){

  double area;
  int i;
 
  //Apply Simpson’s Rule Formula 
  area=fx[0]+fx[n];
  for(i=1; i<n; i++){
    if(i%2==0) area+=2*fx[i];
    else area+=4*fx[i];
  }
  
  //Multiply area by h/3 after converting it to radians 
  double mult_rad=degtorad(h/3.0);
  area=mult_rad*area;

  return area;
}

int main() {

   int n, i;
   double a, b, h, area;
   double *x, *fx, deg, rad;

   //Enter the values for a, b and n and calculate the value of h
   //Note: The interval [a,b] is split up into n subintervals of width h with n an even number.  
   printf("Enter a, b, and n\n");
   scanf("%lf", &a);
   scanf("%lf", &b);
   scanf("%d", &n);
   if (n%2 != 0) {
      printf("n should be even!\n");
      return 1;
   }
   h = (b-a)/n;
 
   //Allocate x and fx vectors where x=[a x1 ... b]
   //and fx =[f(a) f(x1) ... f(b)] and a=x0, b=xn
   x = (double *) malloc((n+1)*sizeof(double));
   fx = (double *) malloc((n+1)*sizeof(double));

  //Calculate the values of x and fx arrays 
  for(i=0; i<=n; i++){
    deg=a+i*h;
    rad=degtorad(deg);
    x[i]=rad;
    fx[i]=func(x[i]);
  }

  //Call simpson's function to find an approximated value for the area
  area=simpson(n, h, fx);

  //Print the approximated and exact value of the integral 
  printf("\nApproximated area by Simpson's rule is: %lf\n", area);
  printf("Exact result is: %lf\n", M_PI/4.0);

  free(x);
  free(fx);

  return 0; 
}

