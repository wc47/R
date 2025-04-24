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

//Generate Random Numbers
  gsl_rng* r = gsl_rng_alloc (gsl_rng_taus);
  int seed = time(NULL);
  gsl_rng_set(r,seed);
  double pi=acos(-1);


for(int i=0; i<n; i++)
{ 
  double U1=gsl_rng_uniform(r);
    if(i%1==0)
    {

    }
    else if(i%2 != 0)
    {

    }
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
int main( int argc, char *argv[])
{
  if(argc<6)
  {
    printf("Not enough Arguments\n");
    return 0;
  }

  double n=atof(argv[1]);
  double mu=atof(argv[2]);
  double nu=atof(argv[3]);
  double kappa1=atof(argv[4]);
  double kappa2=atof(argv[5]);
  double lambda=atof(argv[6]);
  int ncols=n;
  int nrows=2;
  double samples[nrows][ncols];
  //double **samples = malloc(nrows * sizeof *samples + (nrows * (ncols * sizeof **samples)));
  double bestmode[2];
  if(kappa1 * kappa2 >=pow(lambda, 2.0))
  {
    bestmode[0]=mu;
    bestmode[1]=nu;
  }
  else if(kappa1 * kappa2 < pow(lambda,2.0))
  {
    double cospsi= kappa2/abs(lambda) * sqrt((pow(lambda, 2.0)+pow(kappa1, 2.0))/(pow(lambda, 2.0)+pow(kappa2, 2.0)));
    double cosphi= kappa1/abs(lambda) * sqrt((pow(lambda, 2.0)+pow(kappa2, 2.0))/(pow(lambda, 2.0)+pow(kappa1, 2.0)));
    double psi0=acos(cospsi);
    double phi0=acos(cosphi);

      if(lambda > 0)
      {
        double opmod1psi= phi0 + mu;
        double opmod1phi= psi0 + nu;
        double opmod2psi= -phi0 - mu;
        double opmod2phi= -psi0 - nu;

          if(dvonmises( opmod1psi, opmod1phi, mu, nu, kappa1, kappa2, lambda) > dvonmises(opmod2psi, opmod2phi, mu, nu, kappa1, kappa2, lambda))
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
      else if(lambda <= 0)
      {
         double opmod3psi= phi0 + mu;
         double opmod3phi= -psi0 + nu;
         double opmod4psi= -phi0 + mu;
         double opmod4phi= psi0 + nu;

         if(dvonmises(opmod3psi,opmod3phi, mu, nu, kappa1, kappa2, lambda) > dvonmises(opmod4psi, opmod4phi, mu, nu, kappa1, kappa2, lambda))
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
  double alpha=dvonmises(bestmode[0],bestmode[1], mu, nu, kappa1, kappa2, lambda);
  gsl_rng* r = gsl_rng_alloc (gsl_rng_taus);
  int seed = time(NULL);
  gsl_rng_set(r,seed);
  int cl = 2;
  double pi=acos(-1);
  int i = 0;
  while(i <= n){
      double x[2];
      for(int j=0; j<2; j++){
        x[j]=gsl_ran_flat(r,-pi,pi);
      }
      double Rran=gsl_rng_uniform(r);
      double u=log(Rran);
      double test=u + logEnv() - log(alpha); 
      if(test <= dvonmises(x[0],x[1],mu,nu,kappa1,kappa2,lambda)){
        samples[0][i]=x[0];
        samples[1][i]=x[1];
        i++;
      }
  }
  for(int a=0; a<n; a++)
  {
    for(int m=0; m<=1; m++)
    {
      printf("%f0.2",samples[m][a]);
      printf(" ");
    }
  printf("\n");

  }

  return 0;
}
