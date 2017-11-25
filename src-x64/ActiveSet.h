#ifndef ACTIVESET_H
#define ACTIVESET_H

#include "Gauss.h"
#include "Spheres.h"


#include <cstdlib> //malloc

class ActiveSet
{
    public:
        ActiveSet();
        ActiveSet(double cste, int p, int t);

        int getLabel();
        Gauss& getCost(); ///DANGER
        Spheres getSpheres();

        void newSphere_in(double new_mb);
        void newSphere_out(Sphere sph);

        int pruning(int pruntype);
        void show();

    private:

        int m_label;
        Gauss m_current_cost;  /// pointer to the cost associated to the current piece
        Spheres m_spheres;
};


/// chained list of element. element = Object ActiveSet piece + element* nxt

typedef struct element element;
struct element
{
    ActiveSet piece;
    struct element *nxt;
    element() : piece(ActiveSet()), nxt(NULL) {}
};

typedef element* ListE;


#endif // ACTIVESET_H



