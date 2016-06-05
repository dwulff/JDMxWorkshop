#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
double rnf(int a = 0, int b = 1){
  double r = double(std::rand()) / RAND_MAX;
  if(a == 0 && b == 1) return r;
  return r * (b - a) + a;
}

// [[Rcpp::export]]
std::vector<int> sq(int len, int start = 0){
  int i;
  std::vector<int> sequence;
  for(i = start; i < len; i++) sequence.push_back(i);
  return sequence;
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

/////////////////////////////////////////////////////////////////////////////
// Problem generator & handler
/////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
NumericMatrix problemGenerator(int nproblem, int noutcomeA, int noutcomeB) {
  int i,j;
  std::vector<double> ps;
  NumericMatrix problems(nproblem,noutcomeA*2+noutcomeB*2);
  for(i = 0; i < nproblem; i++){
    ps = nrnf(noutcomeA);
    for(j = 0; j < noutcomeA; j++){
      problems(i,j) = rnf(0,100);
      problems(i,j + noutcomeA) = ps[j];      
    }
    ps = nrnf(noutcomeB);
    for(j = 0; j < noutcomeB; j++){
      problems(i,j + noutcomeA * 2) = rnf(0,100);
      problems(i,j + noutcomeA * 2 + noutcomeB) = ps[j];      
    }
  }
  return problems;
}

bool incrCompare(const std::pair<double, double>& firstElem, const std::pair<double, double>& secondElem) {
  return firstElem.first < secondElem.first;
}
bool decrCompare(const std::pair<double, double>& firstElem, const std::pair<double, double>& secondElem) {
  return firstElem.first > secondElem.first;
}
std::vector< pair<double, double> > mysort(std::vector< pair<double, double> > pairs, bool decreasing = true){
  if(decreasing == true)  std::sort(pairs.begin(), pairs.end(), decrCompare);
  if(decreasing == false) std::sort(pairs.begin(), pairs.end(), incrCompare);
  return pairs;
}

// [[Rcpp::export]]
std::vector<double> arrange(std::vector<double> opt){
  int i, n = opt.size(), outn = opt.size() / 2;
  pair<double, double> event;
  std::vector< pair<double, double> > plus, minus;
  std::vector<double> os, ps, all;
  for(i = 0; i < n / 2; i++){
    event.first  = opt[i];
    event.second = opt[i + outn];
    if(opt[i] > 0){
      plus.push_back(event);
    } else {
      minus.push_back(event);
    }
  }
  plus  = mysort(plus,true);
  minus = mysort(minus,false);
  std::vector< pair<double, double> >::const_iterator it;
  for (it = minus.begin(); it != minus.end(); ++it){
    os.push_back(it->first);
    ps.push_back(it->second);
  }
  for (it = plus.begin(); it != plus.end(); ++it){
    os.push_back(it->first);
    ps.push_back(it->second);
  }
  all.insert(all.end(), os.begin(), os.end());
  all.insert(all.end(), ps.begin(), ps.end());
  all.push_back(minus.size());
  return all;
}



// [[Rcpp::export]]
GenericVector transformProblems(NumericMatrix problems, int nA = 0){
  int i, j, nr = problems.nrow(), nc = problems.ncol();
  double tmp;
  tmp = 0;
  if(nA == 0) {
    nA = nc / 2;
  } else  {
    nA = nA * 2;
  }
  NumericMatrix  As(nr, 1 + nA); 
  NumericMatrix  Bs(nr, 1 + nc - nA);
  std::vector<double> A,B,Aar,Bar;
  GenericVector trProblems(2);
  for(i = 0; i < nr; i++){
    for(j = 0;  j < nA; j++) A.push_back(problems(i,j));
    for(j = nA; j < nc; j++) B.push_back(problems(i,j));
    Aar = arrange(A);
    Bar = arrange(B);
    for(j = 0;  j < Aar.size(); j++){ 
      tmp = Aar[j];
      As(i,j) = tmp;
    }
    for(j = 0;  j < Bar.size(); j++){ 
      tmp = Bar[j];
      Bs(i,j) = tmp;
    }
    A.clear();
    B.clear();
  }
  trProblems[0] = As;
  trProblems[1] = Bs;
  return trProblems;
}


/////////////////////////////////////////////////////////////////////////////
// CPT helper
/////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
std::vector<double> cump(std::vector<double> ps){
  int i, n = ps.size();
  double cum = 0;
  std::vector<double> cump;
  for(i = 0; i < n; i++){
    cum += ps[i];
    cump.push_back(cum);
  }
  if(cump.back() > 1) cump[i - 1] = 1.;
  return cump;
}

// [[Rcpp::export]]
double v_f(double v, double fct, double xp){
  if(v < 0) return -1 * fct * pow(abs(v), xp);
  return pow(abs(v), xp);
}

// [[Rcpp::export]]
double w_ge(double p, double fct, double xp){
  double nom = fct * pow(p, xp);
  double denom = pow(1.0 - p, xp);
  return  nom / (nom + denom);
}

// [[Rcpp::export]]
double w_tk(double p, double xp){
  double nom = pow(p, xp);
  double denom = pow(1.0 - p, xp);
  return  nom / pow((nom + denom),1/xp);
}

// [[Rcpp::export]]
double w_p(double p, double fct, double xp){
  return  exp(-fct*pow((-log(p)),xp));
}


/////////////////////////////////////////////////////////////////////////////
// Models
/////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
double utility_tk(std::vector<double> par, NumericVector opt){
  int i, n = opt.size(), no = (opt.size() - 1)/2;
  int nneg = opt[n - 1];
  int npos = no - nneg;
  double v, w, nw, ut = 0;
  std::vector<double> vs, cmp, ps;
  if(nneg > 0){
    for(i = 0; i < nneg; i++){
      v = v_f(double(opt[i]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + no]);
    }
    cmp = cump(ps);
    w   = w_tk(cmp[0], par[0]);
    ut += w * vs[0];
    for(i = 1; i < nneg; i++){
      nw = w_tk(cmp[i], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
    vs.clear();
    ps.clear();
  }
  if(npos > 0){
    for(i = 0; i < npos; i++){
      v = v_f(double(opt[i + nneg]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + nneg + no]);
    }
    cmp = cump(ps);
    w   = w_tk(cmp[0], par[0]);
    ut += w * vs[0];
    for(i = 1; i < npos; i++){
      nw = w_tk(cmp[i], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
  }  
  return ut;
}


// [[Rcpp::export]]
double utility_ge(std::vector<double> par, NumericVector opt){
  int i, n = opt.size();
  int no = (n - 1)/2;
  int nneg = opt[n - 1];
  int npos = no - nneg;
  double v, w, nw, ut = 0;
  std::vector<double> vs, cmp, ps;
  if(nneg > 0){
    for(i = 0; i < nneg; i++){
      v = v_f(double(opt[i]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + no]);
    }
    cmp = cump(ps);
    w   = w_ge(cmp[0], par[1], par[0]);
    ut += w * vs[0];
    for(i = 1; i < nneg; i++){
      nw = w_ge(cmp[i], par[1], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
    vs.clear();
    ps.clear();
  }
  if(npos > 0){
    for(i = 0; i < npos; i++){
      v = v_f(double(opt[i + nneg]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + nneg + no]);
    }
    cmp = cump(ps);
    w   = w_ge(cmp[0], par[1], par[0]);
    ut += w * vs[0];
    for(i = 1; i < npos; i++){
      nw = w_ge(cmp[i], par[1], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
  }  
  return ut;
}

// [[Rcpp::export]]
double utility_p(std::vector<double> par, NumericVector opt){
  int i, n = opt.size();
  int no = (n - 1)/2;
  int nneg = opt[n - 1];
  int npos = no - nneg;
  double v, w, nw, ut = 0;
  std::vector<double> vs, cmp, ps;
  if(nneg > 0){
    for(i = 0; i < nneg; i++){
      v = v_f(double(opt[i]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + no]);
    }
    cmp = cump(ps);
    w   = w_p(cmp[0], par[1], par[0]);
    ut += w * vs[0];
    for(i = 1; i < nneg; i++){
      nw = w_p(cmp[i], par[1], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
    vs.clear();
    ps.clear();
  }
  if(npos > 0){
    for(i = 0; i < npos; i++){
      v = v_f(double(opt[i + nneg]),1,1);
      vs.push_back(v);
      ps.push_back(opt[i + nneg + no]);
    }
    cmp = cump(ps);
    w   = w_p(cmp[0], par[1], par[0]);
    ut += w * vs[0];
    for(i = 1; i < npos; i++){
      nw = w_p(cmp[i], par[1], par[0]);
      ut += vs[i] * (nw - w);
      w = nw;
    }
  }  
  return ut;
}


/////////////////////////////////////////////////////////////////////////////
// Response models
/////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]  
double choiceRule(double phi, double utA, double utB){
  return 1 / (1 + exp(phi * (utB - utA)));
}  

// [[Rcpp::export]]
double cpt_lik_tk(std::vector<double> par, GenericVector problems, std::vector<int> choices, double limit = .0001){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, llik = 0;
  NumericVector A, B;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_tk(par, A);
    utB = utility_tk(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    if(pA < limit) pA = limit;
    if((1-pA) < limit) pA = 1 - limit;
    if(choice == 0){
      llik += log(pA);
    } else {
      llik += log(1 - pA);
    }
  }  
  return -llik;
} 

// [[Rcpp::export]]
double cpt_lik_ge(std::vector<double> par, GenericVector problems, std::vector<int> choices, double limit = .0001){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, llik = 0;
  NumericVector A, B;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_ge(par, A);
    utB = utility_ge(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    if(pA < limit) pA = limit;
    if((1-pA) < limit) pA = 1 - limit;
    if(choice == 0){
      llik += log(pA);
    } else {
      llik += log(1 - pA);
    }
  }  
  return -llik;
} 

// [[Rcpp::export]]
double cpt_lik_p(std::vector<double> par, GenericVector problems, std::vector<int> choices, double limit = .0001){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, llik = 0;
  NumericVector A, B;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_p(par, A);
    utB = utility_p(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    if(pA < limit) pA = limit;
    if((1-pA) < limit) pA = 1 - limit;
    if(choice == 0){
      llik += log(pA);
    } else {
      llik += log(1 - pA);
    }
  }  
  return -llik;
} 


/////////////////////////////////////////////////////////////////////////////
// Choice generators
/////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
std::vector<int> cpt_randchoice_tk(std::vector<double> par, GenericVector problems){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, r;
  NumericVector A, B;
  std::vector<int> choices;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_tk(par, A);
    utB = utility_tk(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    r = double(std::rand()) / RAND_MAX;
    if(r < pA){
      choices.push_back(0);
    } else {
      choices.push_back(1);
    }
  }  
  return choices;
} 


// [[Rcpp::export]]
std::vector<int> cpt_randchoice_ge(std::vector<double> par, GenericVector problems){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, r;
  NumericVector A, B;
  std::vector<int> choices;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_ge(par, A);
    utB = utility_ge(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    r = double(std::rand()) / RAND_MAX;
    if(r < pA){
      choices.push_back(0);
    } else {
      choices.push_back(1);
    }
  }  
  return choices;
} 


// [[Rcpp::export]]
std::vector<int> cpt_randchoice_p(std::vector<double> par, GenericVector problems){
  NumericMatrix As = problems[0], Bs = problems[1];
  int i, choice, n = As.nrow();
  double utA, utB, pA, r;
  NumericVector A, B;
  std::vector<int> choices;
  for(i = 0; i < n; i++){
    //cout << llik << "\n";
    A = As(i,_);
    B = Bs(i,_);
    choice = choices[i];
    utA = utility_p(par, A);
    utB = utility_p(par, B);
    pA  = choiceRule(par.back(),utA,utB);
    r = double(std::rand()) / RAND_MAX;
    if(r < pA){
      choices.push_back(0);
    } else {
      choices.push_back(1);
    }
  }  
  return choices;
} 