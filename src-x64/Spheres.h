#ifndef SPHERES_H
#define SPHERES_H

#include<vector>

struct Sphere
{
        double R2;
        std::vector<double> C;

        Sphere(std::vector<double> Center, double radius2): R2(radius2), C(Center){}
};



class Spheres
{
    public:
        Spheres();

        void addSphere_in(Sphere new_sphere);
        void addSphere_out(Sphere new_sphere);

        std::vector<Sphere> getSphere_in();
        std::vector<Sphere> getSphere_out();

        static double d(std::vector<double> A, std::vector<double> B);


        void show();

    private:
        std::vector<Sphere> sphere_out;
        std::vector<Sphere> sphere_in;
};

#endif // SPHERES_H
