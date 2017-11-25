#include "Omega.h"
#include "Gauss.h"

#include "ActiveSet.h"

#include<math.h>
#include<iostream>

using namespace std;

Omega::Omega(double b) : beta(b), active(NULL){}


//####### PELT #######// //####### PELT #######// //####### PELT #######//
//####### PELT #######// //####### PELT #######// //####### PELT #######//

void Omega::pelt(Data data, int type, int show)
{
    ///data downloading
    int p = data.getp();
    int n = data.getn();
    std::vector<Point> points = data.getvectPoints();

    double mb = 0; ///initial cost constant

    for (int t = 0 ; t < n ; t++)
    {
		if(show != 0){if(div(t,show).rem == 0){std::cout<<"addPoint : "<<t<<endl;}}
        addPoint(points[t],mb,p,t);

        pruning(type);
        number();
    }
}

//####### addFunction #######// //####### addFunction #######// //####### addFunction #######//
//####### addFunction #######// //####### addFunction #######// //####### addFunction #######//

void Omega::addPoint(Point pt, double& mb, int p, int t)
{
    ///1 ### LOCAL ### Creating a new ActiveSet with label t
    element* newElt = new element();
        newElt->piece = ActiveSet(mb,p,t);
        newElt->nxt = active;
    active = newElt;

/// loop initialization
    element* tmp = active;
    tmp->piece.getCost().addPoint(pt); ///adding a new element in front of the linked list

/// First element initialization
    double mini = tmp->piece.getCost().minimum(); /// value of the minimum = mb
    int label = tmp->piece.getLabel();  /// the lastChangePoint : label of the global minimum = t
    vector<double> mean = tmp->piece.getCost().getC(); /// the global minimum = pt

    tmp = tmp->nxt;

    while(tmp!=NULL)
    {
        ///add the new point pt
            tmp->piece.getCost().addPoint(pt); ///adding a new element in front of the linked list

        ///if obtained min cost < mini
            if(tmp->piece.getCost().minimum()<mini){
                    mini = tmp->piece.getCost().minimum();
                    label = tmp->piece.getLabel();
                    mean = tmp->piece.getCost().getC();
            }
            tmp = tmp->nxt;
    }
///filling the vectors
    lastChangePoint.push_back(label); ///adding a label to changepoint
    means.push_back(mean);

    mb = mini + beta;  ///new mb = global minimum step t + beta

    ///4 ### GLOBAL ### adding a spheres_in to each ActiveSet
    tmp = active;
    while(tmp!=NULL){
        tmp->piece.newSphere_in(mb);
        tmp = tmp->nxt;
    }
    ///5 ### GLOBAL ### adding spheres_out to the last ActiveSet = active
    tmp = active->nxt;

    while(tmp!=NULL){
        int S = tmp->piece.getSpheres().getSphere_in().size();
        Sphere S1 = active->piece.getSpheres().getSphere_in()[0];
        Sphere S2 = tmp->piece.getSpheres().getSphere_in()[S-1];

        if(Spheres::d(S1.C,S2.C)<sqrt(S1.R2)+sqrt(S2.R2)){active->piece.newSphere_out(S2);} ///adding an exclusion sphere if intersection
        tmp = tmp->nxt;
    }
}



//####### pruning #######// //####### pruning #######// //####### pruning #######//
//####### pruning #######// //####### pruning #######// //####### pruning #######//

void Omega::pruning(int pruntype)
{
    pruningType.push_back(0);
    pruningDelay.push_back(-1);

    element* tmp = active;
    int type = 0;
    type = tmp->piece.pruning(pruntype);
    if(type > 0)
        {
            active = tmp->nxt;
            pruningType[tmp->piece.getLabel()]=type;
            pruningDelay[tmp->piece.getLabel()]=pruningDelay.size()-tmp->piece.getLabel()-1;
        }

    element* tmpnxt = active->nxt;

    while(tmpnxt!=NULL)
        {
            type = tmpnxt->piece.pruning(pruntype);
            if(type > 0){
                    tmp->nxt = tmpnxt->nxt;
                    pruningType[tmpnxt->piece.getLabel()]=type;
                    pruningDelay[tmpnxt->piece.getLabel()]=pruningDelay.size()-tmpnxt->piece.getLabel()-1;
            }
            else{tmp = tmp->nxt;}
            tmpnxt = tmpnxt->nxt;
        }
}


void Omega::number()
{
    int number = 0;
    element* tmp = active;
    while(tmp!=NULL){
        number = number +1;
        tmp = tmp->nxt;
    }
    nbSet.push_back(number);
}

///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///
///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///

void Omega::show()
{

    std::cout << "----------------------------------------------------------------------------------"<<endl;
    std::cout << "---------------------------------    SPHERES   -----------------------------------"<<endl;
    std::cout << "----------------------------------------------------------------------------------"<<endl;
    element* tmp = active;
   while(tmp!=NULL)
   {
       cout<<"LABEL : "<<tmp->piece.getLabel()<<endl;
      tmp->piece.getSpheres().show();
      tmp = tmp->nxt;
   }
   std::cout <<endl;

    std::cout << "lastChangePoint -- PRUNING TYPE -- PRUNING DELAY -- ACTIVE LENGTH"<<endl;

           for (unsigned int i = 0 ; i < pruningType.size() ; i++)
    {
          std::cout<<i<<" -- "<<lastChangePoint[i]<<" -- "<<pruningType[i]<<" -- "<<pruningDelay[i]<<" -- "<<nbSet[i]<<endl;
    }


  //  std::vector<int> vec(pruningDelay.size(),0);
  //
  //  for (int i = 0 ; i < pruningDelay.size() ; i++)
  //  {
  //      for (int j = 0 ; j < pruningDelay[i] ; j++){if(i+j<pruningDelay.size()){vec[i+j]=vec[i+j]+1;}}
  //  }
  //  std::cout << "TESTTEST DELAY : "<<endl;
  //  for (int i = 0 ; i < pruningDelay.size() ; i++)
  //  {
  //      std::cout<<vec[i]<<" -- ";
  //  }
  //  std::cout <<endl;

    double sum = 0;
    for (unsigned int i = 0 ; i < nbSet.size() ; i++){sum = sum + nbSet[i];}
    std::cout << "ACTIVE LENGTH sum : "<<sum<<endl;

}


std::vector<int> Omega::getlastChangePoint(){return(lastChangePoint);}
std::vector<std::vector<double> > Omega::getmeans(){return(means);}
std::vector<int> Omega::getnbSet(){return(nbSet);}
std::vector<int> Omega::getpruningType(){return(pruningType);}
std::vector<int> Omega::getpruningDelay(){return(pruningDelay);}


