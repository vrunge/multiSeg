#include "ActiveSet.h"
#include "Gauss.h"
#include "Spheres.h"

#include "math.h"
#include <iostream>

using namespace std; ///pour utiliser std::cout et endl de iostream

ActiveSet::ActiveSet(){}

ActiveSet::ActiveSet(double cste, int p, int t)
{
    m_label = t;
    m_current_cost = Gauss(cste, p);
    m_spheres = Spheres();
}

int ActiveSet::getLabel(){return(m_label);}
Gauss& ActiveSet::getCost(){return(m_current_cost);}
Spheres ActiveSet::getSpheres(){return(m_spheres);}


///##### newSphere #####/// ///##### newSphere #####/// ///##### newSphere #####/// ///##### newSphere #####///
///##### newSphere #####/// ///##### newSphere #####/// ///##### newSphere #####/// ///##### newSphere #####///


void ActiveSet::newSphere_in(double new_mb)
{
    double radius2 = 0;
    radius2 = (new_mb-m_current_cost.getmb()-m_current_cost.getAV())/m_current_cost.getA();

    Sphere newSphere = Sphere(m_current_cost.getC(),radius2);
    m_spheres.addSphere_in(newSphere);
}


void ActiveSet::newSphere_out(Sphere sph){m_spheres.addSphere_out(sph);}


///##### pruning #####/// ///##### pruning #####/// ///##### pruning #####/// ///##### pruning #####///
///##### pruning #####/// ///##### pruning #####/// ///##### pruning #####/// ///##### pruning #####///


int ActiveSet::pruning(int pruntype)
{
    int response = 0;
    std::vector<Sphere> sph = m_spheres.getSphere_in();
    int S = sph.size();

///PRUNING TYPE 1 = R2 < 0
    if(sph[S-1].R2<0  && pruntype >= 1){response = 1;}

///PRUNING TYPE 2 = INTERSECTION = EMPTYSET;
    if(response == 0 && pruntype >= 2)
        {
    double D = 0;
        for(int i = 0 ; i < S ; i++)
            {
                D = Spheres::d(sph[i].C,sph[S-1].C);
                if(D >= sqrt(sph[i].R2) + sqrt(sph[S-1].R2)){response = 2;}
            }
            ///OR = reduced to 1 or a few tests
        }

///PRUNING TYPE 3 = CANDIDATE INSIDE A SPHERE OF EXCLUSION
    if(response == 0 && pruntype >= 3)
        {
    std::vector<Sphere> sph_out = m_spheres.getSphere_out();
    double E = 0;
        for(unsigned int i = 0 ; i < sph_out.size() ; i++)
            {
                E = Spheres::d(sph_out[i].C,sph[S-1].C);
                //std::cout<<"P3 : "<<E<<" -- "<<sqrt(sph_out[i].R2) - sqrt(sph[S-1].R2)<<endl;
                if(E <= sqrt(sph_out[i].R2) - sqrt(sph[S-1].R2)){response = 3;}
            }
        }

    return(response);
}


///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///
///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///


void ActiveSet::show()
{
    std::cout<<"LABEL : "<<m_label<<" ";
    m_current_cost.show();
    m_spheres.show();
}

