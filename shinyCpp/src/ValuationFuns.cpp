#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector SUMp(GenericVector s){
  int p,n,i,np;
  double sum; 
  np = s.size();
  NumericVector ss;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
     ss    = s[p];
     n     = ss.size();
     sum   = 0;
     for(i = 0; i < n; i++){
       sum += ss[i];
       }
     vs[p] = sum / n;
    }
  return vs;
  }

// [[Rcpp::export]]
double roundd(double x, double d){
  return   round(x * pow(10,d)) / pow(10,d);
  }


// [[Rcpp::export]]
NumericVector SUMpr(GenericVector s, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,n,i,np;
  double sum,v; 
  np = s.size();
  LogicalVector t1,t2;
  NumericVector ss,rv;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
    ss    = s[p];
    n     = ss.size();
    sum   = 0;
    for(i = 0; i < n; i++){
      sum += ss[i];
      }
    v =sum / n;
    do{
      rv = rnorm(1,v,sd);
      t1 = rv < a;
      t2 = rv > b;
      } while(t1[0] || t2[0]);  
    vs[p] = rv[0];
    }
  return vs;
  }

// [[Rcpp::export]]
NumericVector SUMf(GenericVector s, NumericVector evs, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,i,n,np;
  double sum,v;
  NumericVector ev,ll,ss;
  ll = 0;
  np = s.size();
  for(p = 0; p < np; p++){
     ss    = s[p];
     n     = ss.size();
     sum   = 0;
     for(i = 0; i < n; i++){
       sum += ss[i];
       }
     v     = sum / n;
     ev    = evs[p];
     ll   += log(dnorm(ev,v,sd)/(pnorm(b,v,sd) - pnorm(a,v,sd)));
    }
  return -1 * ll;
  }

// [[Rcpp::export]]
NumericVector VUMp(GenericVector s, double phi){
  int p,i,n,np;
  double v;
  np = s.size();
  NumericVector ss;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = ss[0];
    for(i = 1; i < n; i++){
      v = (1-pow(i+1,-phi)) * v +  pow(i+1,-phi) * ss[i];  
      }
    vs[p] = v;
    }
  return vs;
  }




// [[Rcpp::export]]
NumericVector VUMpuni(GenericVector s, double d, double phia = .0001, double phib = 2, double nsteps = 10000){
  int p,i,n,np;
  double v, phi;
  np = s.size();
  NumericVector ss;
  std::set<double> unis;
  double left = log(phia), right = log(phib);
  double stepsize = (right - left) / nsteps;
  NumericVector nuni(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    unis.clear();
    for(double lphi = left; lphi <= right; lphi += stepsize){
      phi = exp(lphi);
      v  = ss[0];
      for(i = 1; i < n; i++){
        v = (1-pow(i+1,-phi)) * v +  pow(i+1,-phi) * ss[i];  
        }
      unis.insert(roundd(v,d));
      }
    nuni[p] = unis.size();
    }
  return nuni;
  }


// [[Rcpp::export]]
NumericVector VUMpr(GenericVector s, double phi, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,i,n,np;
  double v;
  np = s.size();
  LogicalVector t1,t2;
  NumericVector ss,rv;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = ss[0];
    for(i = 1; i < n; i++){
      v = (1-pow(i+1,-phi)) * v +  pow(i+1,-phi) * ss[i];  
      }
    do{
      rv = rnorm(1,v,sd);
      t1 = rv < a;
      t2 = rv > b;
      } while(t1[0] || t2[0]);  
    vs[p] = rv[0];
    }
  return vs;
  }


// [[Rcpp::export]]
NumericVector VUMf(GenericVector s, NumericVector evs, double phi, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,i,n,np;
  double v;
  NumericVector ev,ll,ss;
  ll = 0;
  np = s.size();
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = ss[0];
    for(i = 1; i < n; i++){
      v = (1-pow(i+1,-phi)) * v +  pow(i+1,-phi) * ss[i];  
      }
    ev    = evs[p];
    ll   += log(dnorm(ev,v,sd)/(pnorm(b,v,sd) - pnorm(a,v,sd)));
    }
  return -1 * ll;
  }


// [[Rcpp::export]]
NumericVector SWIMp(GenericVector s, double zeta){
  int p,i,n,lim,np;
  double v;
  np = s.size();
  NumericVector ss;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = 0;
    if(zeta > n){
      lim = 0;
    } else {
      lim = n - zeta;
    };
    for(i = lim; i < n; i++){
      v = v + ss[i];  
    }
    vs[p] = v/(n-lim);
  }
  return vs;
}


// [[Rcpp::export]]
NumericVector SWIMpuni(GenericVector s, double d){
  int p,i,n,lim,np;
  double v;
  np = s.size();
  NumericVector ss;
  NumericVector vs(np);
  std::set<double> unis;
  NumericVector nuni(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    for(int zeta = 1; zeta <= n; zeta++){
      v  = 0;
      lim = n - zeta;
      for(i = lim; i < n; i++){
        v = v + ss[i];  
        }
      unis.insert(roundd(v/(n-lim),d));
      }
    nuni[p] = unis.size();
    }
  return nuni;
  }


// [[Rcpp::export]]
NumericVector SWIMpr(GenericVector s, double zeta, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,i,n,lim,np;
  double v;
  LogicalVector t1,t2;
  np = s.size();
  NumericVector ss,rv;
  NumericVector vs(np);
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = 0;
    if(zeta > n){
      lim = 0;
      } else {
      lim = n - zeta;
      };
    for(i = lim; i < n; i++){
      v = v + ss[i];  
      }
    v = v/(n-lim);
    do{
      rv = rnorm(1,v,sd);
      t1 = rv < a;
      t2 = rv > b;
    } while(t1[0] || t2[0]);  
    vs[p] = rv[0];
    }
  return vs;
  }

// [[Rcpp::export]]
NumericVector SWIMf(GenericVector s, NumericVector evs, double zeta, double sd, NumericVector a = 0, NumericVector b = 100){
  int p,i,n,lim,np;
  double v;
  NumericVector ev,ll,ss;
  ll = 0;
  np = s.size();
  for(p = 0; p < np; p++){
    ss = s[p];
    n  = ss.size();
    v  = 0;
    if(zeta > n){
      lim = 0;
      } else {
      lim = n - zeta;
      };
    for(i = lim; i < n; i++){
      v = v + ss[i];  
      }
    v     = v /(n-lim);
    ev    = evs[p];
    ll   += log(dnorm(ev,v,sd)/(pnorm(b,v,sd) - pnorm(a,v,sd)));
    }
  return -1 * ll;
  }

// [[Rcpp::export]]
int maxN(GenericVector s){
  int p,np,n,mxN;
  np = s.size();
  NumericVector ss;
  mxN = 0;
  for(p = 0; p < np; p++){
    ss   = s[p];
    n    = ss.size();
    if(mxN < n){
      mxN = n;
      }
    }
  return mxN;
  }




// [[Rcpp::export]]
double rnf(int a = 0, int b = 1){
  double r = double(std::rand()) / RAND_MAX;
  if(a == 0 && b == 1) return r;
  return r * (b - a) + a;
}


// [[Rcpp::export]]
std::vector<double> nrnf(int n, int a = 0, int b = 1, bool norm = true){
  int i;
  double r;
  std::vector<double> rs, rsn;
  for(i = 0; i < n; i++){
    r = double(std::rand()) / RAND_MAX;
    rs.push_back(r * (b - a) + a);
  }
  if(norm){
    double sm = 0;
    for(i = 0; i < n; i++){
      sm += rs[i];
    }
    for(i = 0; i < n; i++){
      rsn.push_back(rs[i] / sm);
    }
    return rsn;
  }
  return rs;
}



// [[Rcpp::export]]
NumericMatrix problemGenerator(int nproblem, int noutcome, int a = 0, int b = 100) {
  int i,j;
  std::vector<double> ps;
  NumericMatrix problems(nproblem,noutcome*2);
  for(i = 0; i < nproblem; i++){
    ps = nrnf(noutcome);
    for(j = 0; j < noutcome; j++){
      problems(i,j) = rnf(a,b);
      problems(i,j + noutcome) = ps[j];      
      }
  }
  return problems;
}



// [[Rcpp::export]]
int smpl(std::vector<double> ps){
  int k, n = ps.size();
  double v, sum = 0, r = double(std::rand()) / RAND_MAX;
  for(k = 0; k < n; k++){
    sum += ps[k];
    }
  v = ps[0] / double(sum);
  for(int i = 1; i < n; i++){
    if(v > r){
      return i;      
      }
    v += ps[i] / double(sum);    
    }
  return n;
  }


// [[Rcpp::export]]
GenericVector sampl(NumericMatrix pro, NumericVector nsmp){
  int i,n,p,np,nn,no;
  np = pro.nrow();
  nn = nsmp.size();
  no = pro.ncol()/2;
  GenericVector ss(np);
  NumericVector ns(np);
  std::vector<double> os;
  std::vector<double> ps;
  if(nn != np){
    for(p = 0; p < np; p++){
      ns[p] = nsmp[0];
    }
  } else {
    for(p = 0; p < np; p++){
      ns[p] = nsmp[p];
    }    
  }
  for(p = 0; p < np; p++){
    os.clear();
    ps.clear();
    for(int i = 0; i < no; i++){
      os.push_back(pro(p,i));
      ps.push_back(pro(p,i + no));
      }
    n  = ns[p];
    NumericVector s(n);
    for(i = 0; i < n; i++){
      s[i] = os[smpl(ps)-1];
      }
    ss[p] = s;
  }
  return ss;
}




// [[Rcpp::export]]
double rnd(){
  double i;
  i = std::rand();
  return i;
  }


// [[Rcpp::export]]
DoubleVector runiform(int n, double a, double b){
  double i,v;
  DoubleVector result(n);
  for(i=0;i<n;i++){
    v = std::rand() % 1000001;
    v = v / 1000000;
    v = (v * (b-a)) + a;
    result[i] = v;
    }
  return result;
  }



