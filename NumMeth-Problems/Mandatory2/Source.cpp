
/**************************
Authors Oliver Klinggaard & Benjamin Rømer Hvenegaard

**************************/


// ---------- INCLUDES ---------- //
#include <iostream>
#include <fstream>
#include <math.h>

#include "../Source Code/code/nr3.h"
#include "../Source Code/code/quadrature.h"
#include "../Source Code/code/ludcmp.h"


// ---------- NAMESPACE ---------- //
using namespace std;

// ---------- DEFINES ---------- //
//.... Data ...//
#define w 1.00
#define a (-1/2)*w
#define b (1/2)*w
#define T1 1000
#define T2 500
#define epsilon1 0.80
#define epsilon2 0.60
#define sigma 1.712*pow(10,-9)
#define d 1

//...Functions
#define F (1/2)*(1/pow(pow(d,2)+pow((x-y),2),(1.5)))





Doub func(const Doub x)
{
	return F;
}


int main(void)
{

	//cout << "Function 1 result : " << qtrap(funcc1, a, b) << endl;
	//cout << "Function 2 result : " << qtrap(funcc2, a, b) << endl;
	//cout << "Function 3 result : " << qtrap(funcc3, a, b) << endl;
	cout << "Function 4 result : " << qtrap(funcc4, a, b) << endl;

	cin.ignore();
}