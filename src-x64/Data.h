#ifndef DATA_H
#define DATA_H

#include <string>
#include<vector>

struct Point
{
        std::vector<double> y;
        Point(int p): y(p){}
};

class Data
{
    public:
        Data(std::string newfile);

        int getp();
        int getn();
        std::vector<Point> getvectPoints();

        void show();

    private:
        std::string file;
        int p; ///number of dimensions
        int n; ///number of observations
        std::vector<Point> vectPoints;
};

#endif // DATA_H
