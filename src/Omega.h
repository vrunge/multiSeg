#ifndef OMEGA_H
#define OMEGA_H

#include"Data.h"
#include"ActiveSet.h"

#include<vector>


class Omega
{
    public:
        Omega(double b);

        void pelt(Data data, int type, int show);
        void addPoint(Point pt, double& mb, int p, int t);
        void pruning(int pruntype);

        void number();

        void show();


        std::vector<int> getlastChangePoint();
        std::vector<std::vector<double> > getmeans();
        std::vector<int> getnbSet();
        std::vector<int> getpruningType();
        std::vector<int> getpruningDelay();



    private:
        double beta;
        ListE active; ///linked objects of active Sets

        std::vector<int> lastChangePoint; ///vector of lastChangePoint build by the algorithm
        std::vector<std::vector<double> > means; ///vector of means build by the algorithm

        std::vector<int> nbSet; ///vector of active Points
        std::vector<int> pruningType; ///vector giving the type of the pruning 0, 1, 2, 3, 4 (à = no pruning)
        std::vector<int> pruningDelay; ///vector giving the number of iteration before vanishing
};

#endif // OMEGA_H
