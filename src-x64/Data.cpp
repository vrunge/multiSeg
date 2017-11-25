#include "Data.h"

#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

Data::Data(std::string newfile){

    file = newfile;
    string path(file);
    ifstream openFile(path.c_str());
    string line, temp;
    getline(openFile,line);

    ///nb of columns
    int nbCol(0);
    stringstream ss;
    ss.clear();
    ss << line;
    while (ss >> temp){nbCol = nbCol+1;}
    p = nbCol;

 ///nb of raws
    int nbRaw(1);
    while(getline(openFile, line)){nbRaw = nbRaw+1;}
    n = nbRaw;
    openFile.close();

///downloading in vectPoints
    ifstream openFileAgain(path.c_str());
    for (int i = 0 ; i < n ; i++)
    {
        Point pt(p);
        for (int j = 0 ; j < p ; j++){openFileAgain >> pt.y[j];}
        vectPoints.push_back(pt);
    }
    openFileAgain.close();
}



int Data::getp(){return(p);}
int Data::getn(){return(n);}
std::vector<Point> Data::getvectPoints(){return(vectPoints);}


///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///
///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####/// ///##### show #####///

void Data::show()
{
  std::cout<<"File : "<<file<<endl;
  std::cout<<"number of Columns : "<<p<<endl;
  std::cout<<"number of Raws : "<<n<<endl;
  std::cout<<"Data : "<<endl;
      for (int i = 0 ; i < n ; i++)
    {
        cout<<i<<"  --- ";
        for (int j = 0 ; j < p ; j++){std::cout<<vectPoints[i].y[j]<<"  ";}
        std::cout<<endl;
    }
}
