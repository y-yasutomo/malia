#include <stdio.h>
#include <math.h>

//Half-Normal
double HN_C(double x, double sigma){
  double output;
  output = exp(-pow(x/sigma,2)/2);
  return output;
}//HN_C

double integrate_HN(double lower, double upper, double sigma, int divide){
  int nint;
  nint = 2* divide +1;
  double h;
  h = (upper-lower)/(nint-1);
  
  double coef[nint];
  int i4;
  int i2;
  coef[0] = 1.0;
  coef[nint-1] = 1.0;
  for(i4=2; i4<=nint-1; i4=i4+2)coef[i4-1] = 4.0;
  for(i2=3; i2<=nint-2; i2=i2+2)coef[i2-1] = 2.0;
  
  double fx[nint];
  double tmpx = lower;
  int ix;
  for(ix=0;ix<nint;ix++){
    fx[ix] = HN_C(tmpx, sigma);
    tmpx=tmpx+h;
  }//for(ix)
  
  double mu = 0.0;
  int isum;
  for(isum=0;isum<nint;isum++)mu=mu+coef[isum]*fx[isum];
  mu=mu*h/3.0;
  
  //mu=0.0;
  //double tmp[nint];
  //tmp[0]=0;
  //for(isum=1;isum<nint;isum++)tmp[isum]=tmp[isum-1]+h;
  //for(isum=0;isum<nint;isum++)mu=mu+tmp[isum];
  //mu=h;
  return mu;
}//integrate_HN

double fx_HN(double xmin, double xmax, double sigma, double td, int divide){
  double output;
  output = integrate_HN(xmin,xmax,sigma,divide)/integrate_HN(0, td, sigma, divide);
  return output;
}//double fx_HN

void NLL_HN(double *xmin, double *xmax, double *sigma, double *td, int *divide, int *n, double *output){
  //double fxs[*n];
  //double val_nll;
  int i;
  //for(i=0; i<*n; i++)fxs[i] = log(fx_HR(xmin[i], xmax[i],sigma[i], b[i], *td, *divide));
  for(i=0; i<*n; i++)*output = *output+log(fx_HN(xmin[i], xmax[i],sigma[i], *td, *divide));
  *output = - *output;
  //output[0] = val_nll;
}//NLL_HN

//Hazard-Rate
double HR_C_vec(double *x, double *sigma, double *b ,int *n){
  double output[*n];
  int i;
	for(i=0; i<*n; i++){
	  output[i] = 1.0 - exp(-pow(x[i]/ sigma[i],-( b[i]+1)));
	}
	return output[*n];
}//HR_C_vec

void HR_C_void(double *x, double *sigma, double *b ,int *n, double *output){
  int i;
  for(i=0; i<*n; i++){
    output[i] = 1.0 - exp(-pow(x[i]/ sigma[i],-( b[i]+1)));
  }
}//HR_C_void

double HR_C(double x, double sigma, double b){
  double output;
  output = 1.0 - exp(-pow(x/sigma,-(b+1)));
  return output;
}//HR_C

double integrate_HR(double lower, double upper, double sigma, double b, int divide){
  int nint;
  nint = 2* divide +1;
  double h;
  h = (upper-lower)/(nint-1);
  
  double coef[nint];
  int i4;
  int i2;
  coef[0] = 1.0;
  coef[nint-1] = 1.0;
  for(i4=2; i4<=nint-1; i4=i4+2)coef[i4-1] = 4.0;
  for(i2=3; i2<=nint-2; i2=i2+2)coef[i2-1] = 2.0;
  
  double fx[nint];
  double tmpx = lower;
  int ix;
  for(ix=0;ix<nint;ix++){
    fx[ix] = HR_C(tmpx, sigma, b);
    tmpx=tmpx+h;
  }//for(ix)
  
  double mu = 0.0;
  int isum;
  for(isum=0;isum<nint;isum++)mu=mu+coef[isum]*fx[isum];
  mu=mu*h/3.0;
  
  //mu=0.0;
  //double tmp[nint];
  //tmp[0]=0;
  //for(isum=1;isum<nint;isum++)tmp[isum]=tmp[isum-1]+h;
  //for(isum=0;isum<nint;isum++)mu=mu+tmp[isum];
  //mu=h;
  return mu;
}//integrate_HR

double fx_HR(double xmin, double xmax, double sigma, double b, double td, int divide){
  double output;
  output = integrate_HR(xmin,xmax,sigma,b,divide)/integrate_HR(0, td, sigma, b, divide);
  return output;
}//double fx_HR

void NLL_HR(double *xmin, double *xmax, double *sigma, double *b, double *td, int *divide, int *n, double *output){
  //double fxs[*n];
  //double val_nll;
  int i;
  //for(i=0; i<*n; i++)fxs[i] = log(fx_HR(xmin[i], xmax[i],sigma[i], b[i], *td, *divide));
  for(i=0; i<*n; i++)*output = *output+log(fx_HR(xmin[i], xmax[i],sigma[i], b[i], *td, *divide));
  *output = - *output;
  //output[0] = val_nll;
}//NLL_HR

//Half-Half-Normal
double HHN_C(double x, double sigma, double delta, double cp){
  double output;
  output = exp(-pow((x-cp)/delta,2)/2)*(x<=cp)+exp(-pow((x-cp)/sigma,2)/2)*(x>cp);
  return output;
}//HN_C

double integrate_HHN(double lower, double upper, double sigma,double delta, double cp, int divide){
  int nint;
  nint = 2* divide +1;
  double h;
  h = (upper-lower)/(nint-1);
  
  double coef[nint];
  int i4;
  int i2;
  coef[0] = 1.0;
  coef[nint-1] = 1.0;
  for(i4=2; i4<=nint-1; i4=i4+2)coef[i4-1] = 4.0;
  for(i2=3; i2<=nint-2; i2=i2+2)coef[i2-1] = 2.0;
  
  double fx[nint];
  double tmpx = lower;
  int ix;
  for(ix=0;ix<nint;ix++){
    fx[ix] = HHN_C(tmpx, sigma, delta, cp);
    tmpx=tmpx+h;
  }//for(ix)
  
  double mu = 0.0;
  int isum;
  for(isum=0;isum<nint;isum++)mu=mu+coef[isum]*fx[isum];
  mu=mu*h/3.0;
  
  //mu=0.0;
  //double tmp[nint];
  //tmp[0]=0;
  //for(isum=1;isum<nint;isum++)tmp[isum]=tmp[isum-1]+h;
  //for(isum=0;isum<nint;isum++)mu=mu+tmp[isum];
  //mu=h;
  return mu;
}//integrate_HHN

double fx_HHN(double xmin, double xmax, double sigma, double delta, double cp, double td, int divide){
  double output;
  output = integrate_HHN(xmin,xmax,sigma, delta, cp,divide)/integrate_HHN(0, td, sigma, delta, cp, divide);
  return output;
}//double fx_HHN

void NLL_HHN(double *xmin, double *xmax, double *sigma, double *delta, double *cp, double *td, int *divide, int *n, double *output){
  //double fxs[*n];
  //double val_nll;
  int i;
  //for(i=0; i<*n; i++)fxs[i] = log(fx_HR(xmin[i], xmax[i],sigma[i], b[i], *td, *divide));
  for(i=0; i<*n; i++)*output = *output+log(fx_HHN(xmin[i], xmax[i],sigma[i], delta[i], *cp, *td, *divide));
  *output = - *output;
  //output[0] = val_nll;
}//NLL_HHN

//Half-Hazard-Rate
double HHR_C(double x, double sigma, double b, double delta, double cp){
  double output;
  output = exp(-pow((x-cp)/delta,2)/2)*(x<=cp)+ (1.0 - exp(-pow(abs(x-cp)/sigma,-(b+1))))*(x>cp);
  return output;
}//HHR_C

double integrate_HHR(double lower, double upper, double sigma, double b, double delta, double cp, int divide){
  int nint;
  nint = 2* divide +1;
  double h;
  h = (upper-lower)/(nint-1);
  
  double coef[nint];
  int i4;
  int i2;
  coef[0] = 1.0;
  coef[nint-1] = 1.0;
  for(i4=2; i4<=nint-1; i4=i4+2)coef[i4-1] = 4.0;
  for(i2=3; i2<=nint-2; i2=i2+2)coef[i2-1] = 2.0;
  
  double fx[nint];
  double tmpx = lower;
  int ix;
  for(ix=0;ix<nint;ix++){
    fx[ix] = HHR_C(tmpx, sigma, b, delta, cp);
    tmpx=tmpx+h;
  }//for(ix)
  
  double mu = 0.0;
  int isum;
  for(isum=0;isum<nint;isum++)mu=mu+coef[isum]*fx[isum];
  mu=mu*h/3.0;
  
  //mu=0.0;
  //double tmp[nint];
  //tmp[0]=0;
  //for(isum=1;isum<nint;isum++)tmp[isum]=tmp[isum-1]+h;
  //for(isum=0;isum<nint;isum++)mu=mu+tmp[isum];
  //mu=h;
  return mu;
}//integrate_HHR

double fx_HHR(double xmin, double xmax, double sigma, double b, double delta, double cp, double td, int divide){
  double output;
  output = integrate_HHR(xmin,xmax,sigma,b, delta, cp, divide)/integrate_HHR(0, td, sigma, b, delta, cp, divide);
  return output;
}//double fx_HHR

void NLL_HHR(double *xmin, double *xmax, double *sigma, double *b, double *delta, double *cp, double *td, int *divide, int *n, double *output){
  //double fxs[*n];
  //double val_nll;
  int i;
  //for(i=0; i<*n; i++)fxs[i] = log(fx_HR(xmin[i], xmax[i],sigma[i], b[i], *td, *divide));
  for(i=0; i<*n; i++)*output = *output+log(fx_HHR(xmin[i], xmax[i],sigma[i], b[i], delta[i], *cp, *td, *divide));
  *output = - *output;
  //output[0] = val_nll;
}//NLL_HHR

