#include <iostream>
#include <string>
#include<math.h>

#include "Data.h"
#include "Omega.h"

#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List transfert(std::string file, double beta, int type)
{
  Data data = Data(file);

  data.show();
  Omega omega(beta);
  omega.pelt(data, type);

  return List::create(
     _["lastChangePoint"] = omega.getlastChangePoint(),
     _["means"] = omega.getmeans(),
     _["nbSet"] = omega.getnbSet(),
     _["pruningType"] = omega.getpruningType(),
     _["pruningDelay"] = omega.getpruningDelay()
  );
}