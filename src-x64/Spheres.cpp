#include "Spheres.h"
#include<math.h>
#include<vector>

#include<iostream>

using namespace std;

Spheres::Spheres()
{

}

void Spheres::addSphere_in(Sphere new_sphere){sphere_in.push_back(new_sphere);}
void Spheres::addSphere_out(Sphere new_sphere){sphere_out.push_back(new_sphere);}

std::vector<Sphere> Spheres::getSphere_in(){return(sphere_in);}
std::vector<Sphere> Spheres::getSphere_out(){return(sphere_out);}

void Spheres::show()
{
    std::cout<<"SPHERE IN "<<endl;
    for(unsigned int i = 0 ; i < sphere_in.size() ; i++)
    {
        std::cout<<i<<" : "<<sphere_in[i].R2<<endl;
    }

    std::cout<<"SPHERE OUT "<<endl;
    for(unsigned int i = 0 ; i < sphere_out.size() ; i++)
    {
          std::cout<<i<<" : "<<sphere_out[i].R2<<endl;
    }
}



double Spheres::d(std::vector<double> A, std::vector<double> B)
{
    double D = 0;
    for(unsigned int i = 0 ; i < A.size() ; i++){D = D + (A[i]-B[i])*(A[i]-B[i]);}
    return(sqrt(D));
}

