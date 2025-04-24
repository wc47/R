#include <stdlib.h>            // malloc
#include <stdio.h>             // printf
#include <math.h>              // fabs, sqrt, etc.
#include <time.h>              // time
#include <unistd.h>            // getpid
#include <gsl/gsl_rng.h>       // GNU Scientific Library
#include <gsl/gsl_cdf.h>       // GNU Scientific Library
#include <gsl/gsl_randist.h>   // GNU Scientific Library
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_sf_gamma.h>


double dvonmises(double psi, double phi, double mu, double nu, double kappa1, double kappa2, double lambda){
  //double C=constant(psi, phi, mu, nu, kappa1, kappa2, lambda);
  double ret= exp(kappa1 * cos(psi-mu)+kappa2 * cos(phi-nu)+lambda * sin(psi-mu) * sin(phi-nu));
  return ret;
}

double
logEnv(void)
{
  double pi=acos(-1);
  double dns=log(1.0 / (4.0 * pow( pi, 2.0)));
  return dns;
}



double rejection_sampler(double alpha, int size, double **result, double mu, double nu, double kappa1, double kappa2, double lambda){
  gsl_rng* r = gsl_rng_alloc (gsl_rng_taus);
  int seed = time(NULL);
  gsl_rng_set(r,seed);
  int cl = 2;
  double pi=acos(-1);
  double ret[2][size];
  int i = 0;
  while(i <= size){
      double x[2];
      for(int j=0; j<2; j++){
        x[j]=gsl_ran_flat(r,-pi,pi);
      }
      double Rran=gsl_rng_uniform(r);
      double u=log(Rran);
      double test=u + logEnv() - log(alpha); 
      if(test <= dvonmises(x[0],x[1],mu,nu,kappa1,kappa2,lambda)){
        result[0][i]=x[0];
        result[1][i]=x[1];
        i++;
      }
  }
}

void rsampler(int* n, double* mu, double* nu, double* kappa1, double* kappa2, double* lambda, double* samples)
{
  //printf("%d\n",*n);
  //printf("%f\n",*mu);
  //printf("%f\n",*nu);
  //printf("%f\n",*kappa1);
  //printf("%f\n",*kappa2);
  //printf("%f\n",*lambda);

  int ncols=*n;
  int nrows=2;
  double bestmode[2];
  if(*kappa1 * *kappa2 >=pow(*lambda, 2.0))
  {
    bestmode[0]=*mu;
    bestmode[1]=*nu;
  }
  else if(*kappa1 * *kappa2 < pow(*lambda,2.0))
  {
    double cospsi= *kappa2/abs(*lambda) * sqrt((pow(*lambda, 2.0)+pow(*kappa1, 2.0))/(pow(*lambda, 2.0)+pow(*kappa2, 2.0)));
    double cosphi= *kappa1/abs(*lambda) * sqrt((pow(*lambda, 2.0)+pow(*kappa2, 2.0))/(pow(*lambda, 2.0)+pow(*kappa1, 2.0)));
    double psi0=acos(cospsi);
    double phi0=acos(cosphi);

      if(*lambda > 0)
      {
        double opmod1psi= phi0 + *mu;
        double opmod1phi= psi0 + *nu;
        double opmod2psi= -phi0 + *mu;
        double opmod2phi= -psi0 + *nu;

          if(dvonmises( opmod1psi, opmod1phi, *mu, *nu, *kappa1, *kappa2, *lambda) > dvonmises(opmod2psi, opmod2phi, *mu, *nu, *kappa1, *kappa2, *lambda))
          {
            bestmode[0]=opmod1psi;
            bestmode[1]=opmod1phi;
          }
          else
          {
            bestmode[0]=opmod2psi;
            bestmode[1]=opmod2phi;
          }
      }
      else if(*lambda <= 0)
      {
         double opmod3psi= phi0 + *mu;
         double opmod3phi= -psi0 + *nu;
         double opmod4psi= -phi0 + *mu;
         double opmod4phi= psi0 + *nu;

         if(dvonmises(opmod3psi,opmod3phi, *mu, *nu, *kappa1, *kappa2, *lambda) > dvonmises(opmod4psi, opmod4phi, *mu, *nu, *kappa1, *kappa2, *lambda))
         {
           bestmode[0]=opmod3psi;
           bestmode[1]=opmod3phi;
         }
         else
         {
           bestmode[0]=opmod4psi;
           bestmode[1]=opmod4phi;
         } 
      } 
  }
  //printf("%s\n","Checkpoint4");
  double pi=acos(-1);
  double alpha=(1/(4.0*pow(pi,2)))/dvonmises(bestmode[0],bestmode[1], *mu, *nu, *kappa1, *kappa2, *lambda);
  gsl_rng* r = gsl_rng_alloc (gsl_rng_taus);
  int seed = time(NULL);
  gsl_rng_set(r,seed);
  int cl = 2;
  int i = 0;
  while(i < *n){
      double x[2];
      for(int j=0; j<2; j++){
        x[j]=gsl_ran_flat(r,-pi,pi);
      }
      double Rran=gsl_rng_uniform(r);
      double u=log(Rran);
      double test=u + logEnv() - log(alpha); 
      //printf("%s\n", "Checkpoint5");
      if(test <= log(dvonmises(x[0],x[1],*mu,*nu,*kappa1,*kappa2,*lambda))){
        samples[0+2*i]=x[0];
        samples[1+2*i]=x[1];
        i++;
      }
  }
  gsl_rng_free(r);
}

int main( int argc, char *argv[])
{
  if(argc != 7)
  {
    printf("Wrong Number of Arguments\n");
    return 0;
  }

  int n=atoi(argv[1]);
  double mu=atof(argv[2]);
  double nu=atof(argv[3]);
  double kappa1=atof(argv[4]);
  double kappa2=atof(argv[5]);
  double lambda=atof(argv[6]);
  //printf("%s\n", "Checkpoint1");
  double* samples=(double*) malloc(2*n*sizeof(double));
  //printf("%s\n", "Checkpoint2");
  rsampler(&n, &mu, &nu, &kappa1, &kappa2, &lambda, samples);
  for(int a=0; a<n; a++)
  {
    for(int m=0; m<2; m++)
    {
      printf("%f",samples[m+2*a]);
      printf(" ");
    }
  printf("\n");

  }
  free(samples);
  return 0;
}


 
