// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP shinyCpp_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(rcpp_hello_world());
    return __result;
END_RCPP
}
// SUMp
NumericVector SUMp(GenericVector s);
RcppExport SEXP shinyCpp_SUMp(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    __result = Rcpp::wrap(SUMp(s));
    return __result;
END_RCPP
}
// roundd
double roundd(double x, double d);
RcppExport SEXP shinyCpp_roundd(SEXP xSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    __result = Rcpp::wrap(roundd(x, d));
    return __result;
END_RCPP
}
// SUMpr
NumericVector SUMpr(GenericVector s, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_SUMpr(SEXP sSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(SUMpr(s, sd, a, b));
    return __result;
END_RCPP
}
// SUMf
NumericVector SUMf(GenericVector s, NumericVector evs, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_SUMf(SEXP sSEXP, SEXP evsSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type evs(evsSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(SUMf(s, evs, sd, a, b));
    return __result;
END_RCPP
}
// VUMp
NumericVector VUMp(GenericVector s, double phi);
RcppExport SEXP shinyCpp_VUMp(SEXP sSEXP, SEXP phiSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type phi(phiSEXP);
    __result = Rcpp::wrap(VUMp(s, phi));
    return __result;
END_RCPP
}
// VUMpuni
NumericVector VUMpuni(GenericVector s, double d, double phia, double phib, double nsteps);
RcppExport SEXP shinyCpp_VUMpuni(SEXP sSEXP, SEXP dSEXP, SEXP phiaSEXP, SEXP phibSEXP, SEXP nstepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type phia(phiaSEXP);
    Rcpp::traits::input_parameter< double >::type phib(phibSEXP);
    Rcpp::traits::input_parameter< double >::type nsteps(nstepsSEXP);
    __result = Rcpp::wrap(VUMpuni(s, d, phia, phib, nsteps));
    return __result;
END_RCPP
}
// VUMpr
NumericVector VUMpr(GenericVector s, double phi, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_VUMpr(SEXP sSEXP, SEXP phiSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(VUMpr(s, phi, sd, a, b));
    return __result;
END_RCPP
}
// VUMf
NumericVector VUMf(GenericVector s, NumericVector evs, double phi, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_VUMf(SEXP sSEXP, SEXP evsSEXP, SEXP phiSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type evs(evsSEXP);
    Rcpp::traits::input_parameter< double >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(VUMf(s, evs, phi, sd, a, b));
    return __result;
END_RCPP
}
// SWIMp
NumericVector SWIMp(GenericVector s, double zeta);
RcppExport SEXP shinyCpp_SWIMp(SEXP sSEXP, SEXP zetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type zeta(zetaSEXP);
    __result = Rcpp::wrap(SWIMp(s, zeta));
    return __result;
END_RCPP
}
// SWIMpuni
NumericVector SWIMpuni(GenericVector s, double d);
RcppExport SEXP shinyCpp_SWIMpuni(SEXP sSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    __result = Rcpp::wrap(SWIMpuni(s, d));
    return __result;
END_RCPP
}
// SWIMpr
NumericVector SWIMpr(GenericVector s, double zeta, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_SWIMpr(SEXP sSEXP, SEXP zetaSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< double >::type zeta(zetaSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(SWIMpr(s, zeta, sd, a, b));
    return __result;
END_RCPP
}
// SWIMf
NumericVector SWIMf(GenericVector s, NumericVector evs, double zeta, double sd, NumericVector a, NumericVector b);
RcppExport SEXP shinyCpp_SWIMf(SEXP sSEXP, SEXP evsSEXP, SEXP zetaSEXP, SEXP sdSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type evs(evsSEXP);
    Rcpp::traits::input_parameter< double >::type zeta(zetaSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    __result = Rcpp::wrap(SWIMf(s, evs, zeta, sd, a, b));
    return __result;
END_RCPP
}
// maxN
int maxN(GenericVector s);
RcppExport SEXP shinyCpp_maxN(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< GenericVector >::type s(sSEXP);
    __result = Rcpp::wrap(maxN(s));
    return __result;
END_RCPP
}
// rnf
double rnf(int a, int b);
RcppExport SEXP shinyCpp_rnf(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    __result = Rcpp::wrap(rnf(a, b));
    return __result;
END_RCPP
}
// nrnf
std::vector<double> nrnf(int n, int a, int b, bool norm);
RcppExport SEXP shinyCpp_nrnf(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    Rcpp::traits::input_parameter< bool >::type norm(normSEXP);
    __result = Rcpp::wrap(nrnf(n, a, b, norm));
    return __result;
END_RCPP
}
// problemGenerator
NumericMatrix problemGenerator(int nproblem, int noutcome, int a, int b);
RcppExport SEXP shinyCpp_problemGenerator(SEXP nproblemSEXP, SEXP noutcomeSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type nproblem(nproblemSEXP);
    Rcpp::traits::input_parameter< int >::type noutcome(noutcomeSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    __result = Rcpp::wrap(problemGenerator(nproblem, noutcome, a, b));
    return __result;
END_RCPP
}
// smpl
int smpl(std::vector<double> ps);
RcppExport SEXP shinyCpp_smpl(SEXP psSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector<double> >::type ps(psSEXP);
    __result = Rcpp::wrap(smpl(ps));
    return __result;
END_RCPP
}
// sampl
GenericVector sampl(NumericMatrix pro, NumericVector nsmp);
RcppExport SEXP shinyCpp_sampl(SEXP proSEXP, SEXP nsmpSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type pro(proSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type nsmp(nsmpSEXP);
    __result = Rcpp::wrap(sampl(pro, nsmp));
    return __result;
END_RCPP
}
// rnd
double rnd();
RcppExport SEXP shinyCpp_rnd() {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    __result = Rcpp::wrap(rnd());
    return __result;
END_RCPP
}
// runiform
DoubleVector runiform(int n, double a, double b);
RcppExport SEXP shinyCpp_runiform(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    __result = Rcpp::wrap(runiform(n, a, b));
    return __result;
END_RCPP
}
