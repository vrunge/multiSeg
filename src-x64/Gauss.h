#ifndef GAUSS_H
#define GAUSS_H

#include "Data.h"
#include<vector>


class Gauss
{
    public:
        Gauss();
        Gauss(double cste, int p);

        int getA();
        std::vector<double> getC();
        double getAV();
        double getmb();

        double minimum();
        double point_eval(Point const& pt);
        void addPoint(Point const& pt);

        void show();

    private:
        int A;
        std::vector<double> C;
        double AV;
        double mb;
};

#endif // GAUSS_H
