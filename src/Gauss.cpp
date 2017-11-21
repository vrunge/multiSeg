#include "Gauss.h"

#include <iostream>
#include<vector>

using namespace std; ///pour utiliser std::cout et endl de iostream

/// COST = A*sum[(C[i]-theta)^2] + AV + mb

Gauss::Gauss(){}
Gauss::Gauss(double cste, int p): A(0), C(vector<double>(p,0)), AV(0), mb(cste){}

int Gauss::getA(){return(A);}
std::vector<double> Gauss::getC(){return(C);}
double Gauss::getAV(){return(AV);}
double Gauss::getmb(){return(mb);}

double Gauss::minimum(){return(mb+AV);}
double Gauss::point_eval(Point const& pt)
{
    double response = 0;
    for(unsigned int i = 0 ; i < C.size() ; i++){response = response + (pt.y[i]-C[i])*(pt.y[i]-C[i]);}
    response = A*response + mb + AV;
    return(response);
}



void Gauss::addPoint(Point const& pt)
{
    double tmp = 0;
    for(unsigned int i = 0 ; i < C.size() ; i++){tmp = tmp + (pt.y[i] - C[i])*(pt.y[i] - C[i]);}
    AV = AV + tmp*A/(A+1);
    for(unsigned int i = 0 ; i < C.size() ; i++){C[i] = (A*C[i]+pt.y[i])/(A+1);}
    A = A + 1;
}

///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///
///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///

void Gauss::show()
{
  std::cout << "#GAUSS# #A# "<<A<< " #V# "<<AV<<" #mb# "<<mb;
    for(unsigned int i = 0 ; i < C.size() ; i++){std::cout<<" #C"<<i<<"# "<<C[i];}
    std::cout<<endl;
}


