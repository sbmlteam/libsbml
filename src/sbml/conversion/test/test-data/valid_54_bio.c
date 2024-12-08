#ifdef SIZE_DEFINITIONS
#define N_METABS 6
#define N_ODE_METABS 0
#define N_INDEP_METABS 3
#define N_COMPARTMENTS 1
#define N_GLOBAL_PARAMS 14
#define N_KIN_PARAMS 0
#define N_REACTIONS 6

#define N_ARRAY_SIZE_P  14	// number of parameters
#define N_ARRAY_SIZE_X  3	// number of initials
#define N_ARRAY_SIZE_Y  4	// number of assigned elements
#define N_ARRAY_SIZE_XC 3	// number of x concentration
#define N_ARRAY_SIZE_PC 0	// number of p concentration
#define N_ARRAY_SIZE_YC 3	// number of y concentration
#define N_ARRAY_SIZE_DX 3	// number of ODEs 
#define N_ARRAY_SIZE_CT 3	// number of conserved totals

#endif // SIZE_DEFINITIONS

#ifdef TIME
#define T  <set here a user name for the time variable> 
#endif // TIME

#ifdef NAME_ARRAYS
const char* p_names[] = {"compartmentOne", "I", "k__1", "k__2", "k__3", "k__4", "k__5", "k__6", "k__7", "k__8", "k__9", "k__10", "k__11", "k__12",  "" };
const char* x_names[] = {"SimData_1", "SimData_2", "SimData_3",  "" };
const char* y_names[] = {"SimData_4", "SimData_5", "SimData_6", "t",  "" };
const char* xc_names[] = {"SimData_1", "SimData_2", "SimData_3",  "" };
const char* pc_names[] = { "" };
const char* yc_names[] = {"SimData_4", "SimData_5", "SimData_6",  "" };
const char* dx_names[] = {"ODE SimData_1", "ODE SimData_2", "ODE SimData_3",  "" };
const char* ct_names[] = {"CT SimData_4", "CT SimData_5", "CT SimData_6",  "" };
#endif // NAME_ARRAYS

#ifdef INITIAL
x[0] = 1;	//metabolite 'SimData_1': reactions
x[1] = 1;	//metabolite 'SimData_2': reactions
x[2] = 1;	//metabolite 'SimData_3': reactions
#endif /* INITIAL */

#ifdef FIXED
ct[0] = 0.99999999999999978;	//ct[0] conserved total for 'SimData_4'
ct[1] = 0.99999999999999978;	//ct[1] conserved total for 'SimData_5'
ct[2] = 0.99999999999999978;	//ct[2] conserved total for 'SimData_6'
p[0] = 1;	//compartment 'compartmentOne':fixed
p[1] = 1;	//global quantity 'I':fixed
p[2] = 4.2416;	//global quantity 'k__1':fixed
p[3] = 5.9816;	//global quantity 'k__2':fixed
p[4] = 0.1009;	//global quantity 'k__3':fixed
p[5] = 1.1549;	//global quantity 'k__4':fixed
p[6] = 1.3618;	//global quantity 'k__5':fixed
p[7] = 1.4219;	//global quantity 'k__6':fixed
p[8] = 0.0051;	//global quantity 'k__7':fixed
p[9] = 0.0972;	//global quantity 'k__8':fixed
p[10] = 0.0012;	//global quantity 'k__9':fixed
p[11] = 56.8583;	//global quantity 'k__10':fixed
p[12] = 0.0111;	//global quantity 'k__11':fixed
p[13] = 0.0014;	//global quantity 'k__12':fixed
#endif /* FIXED */

#ifdef ASSIGNMENT
y[0] = ct[0]-x[2];	//metabolite 'SimData_4': reactions
y[1] = ct[1]-x[1];	//metabolite 'SimData_5': reactions
y[2] = ct[2]-x[0];	//metabolite 'SimData_6': reactions
y[3] = T;	//model entity 't':assignment
x_c[0] = x[0]/p[0];	//concentration of metabolite 'SimData_1': reactions
x_c[1] = x[1]/p[0];	//concentration of metabolite 'SimData_2': reactions
x_c[2] = x[2]/p[0];	//concentration of metabolite 'SimData_3': reactions
y_c[0] = y[0]/p[0];	//concentration of metabolite 'SimData_4': reactions
y_c[1] = y[1]/p[0];	//concentration of metabolite 'SimData_5': reactions
y_c[2] = y[2]/p[0];	//concentration of metabolite 'SimData_6': reactions
#endif /* ASSIGNMENT */

#ifdef FUNCTIONS_HEADERS
double FunctionForJ1_1(double sub_0, double volume_0, double param_0, double param_1); 
double FunctionForJ2_1(double param_0, double sub_0, double volume_0, double param_1, double param_2); 
double FunctionForJ3(double sub_0, double volume_0, double param_0, double param_1); 
double FunctionForJ4(double sub_0, double sub_1, double volume_0, double param_0, double param_1); 
double FunctionForJ5(double sub_0, double sub_1, double volume_0, double param_0, double param_1); 
double FunctionForJ6(double sub_0, double sub_1, double volume_0, double param_0, double param_1); 
#endif /* FUNCTIONS_HEADERS */

#ifdef FUNCTIONS
double FunctionForJ1_1(double sub_0, double volume_0, double param_0, double param_1) 	//Function for J1_1
{return  sub_0*(param_0/(sub_0+param_1))/volume_0;} 
double FunctionForJ2_1(double param_0, double sub_0, double volume_0, double param_1, double param_2) 	//Function for J2_1
{return  param_1*sub_0*(param_0/(sub_0+param_2))/volume_0;} 
double FunctionForJ3(double sub_0, double volume_0, double param_0, double param_1) 	//Function for J3
{return  sub_0*(param_1/(sub_0+param_0))/volume_0;} 
double FunctionForJ4(double sub_0, double sub_1, double volume_0, double param_0, double param_1) 	//Function for J4
{return  sub_0*sub_1*(param_0/(sub_1+param_1))/volume_0;} 
double FunctionForJ5(double sub_0, double sub_1, double volume_0, double param_0, double param_1) 	//Function for J5
{return  sub_0*sub_1*(param_1/(sub_1+param_0))/volume_0;} 
double FunctionForJ6(double sub_0, double sub_1, double volume_0, double param_0, double param_1) 	//Function for J6
{return  sub_0*sub_1*(param_1/(sub_1+param_0))/volume_0;} 
#endif /* FUNCTIONS */

#ifdef ODEs
dx[0] = -FunctionForJ1_1(x_c[0], p[0], p[3], p[9])*p[0]+FunctionForJ2_1(p[1], y_c[2], p[0], p[2], p[8])*p[0];
dx[1] = -FunctionForJ3(x_c[1], p[0], p[11], p[5])*p[0]+FunctionForJ4(x_c[0], y_c[1], p[0], p[4], p[10])*p[0];
dx[2] = -FunctionForJ5(x_c[1], x_c[2], p[0], p[13], p[7])*p[0]+FunctionForJ6(x_c[0], y_c[0], p[0], p[12], p[6])*p[0];
#endif /* ODEs */
