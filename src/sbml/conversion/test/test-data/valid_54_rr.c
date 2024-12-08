#ifdef SIZE_DEFINITIONS
#define N_METABS 3
#define N_ODE_METABS 3
#define N_INDEP_METABS 0
#define N_COMPARTMENTS 1
#define N_GLOBAL_PARAMS 14
#define N_KIN_PARAMS 0
#define N_REACTIONS 0

#define N_ARRAY_SIZE_P  14	// number of parameters
#define N_ARRAY_SIZE_X  3	// number of initials
#define N_ARRAY_SIZE_Y  1	// number of assigned elements
#define N_ARRAY_SIZE_XC 3	// number of x concentration
#define N_ARRAY_SIZE_PC 0	// number of p concentration
#define N_ARRAY_SIZE_YC 0	// number of y concentration
#define N_ARRAY_SIZE_DX 3	// number of ODEs 
#define N_ARRAY_SIZE_CT 0	// number of conserved totals

#endif // SIZE_DEFINITIONS

#ifdef TIME
#define T  <set here a user name for the time variable> 
#endif // TIME

#ifdef NAME_ARRAYS
const char* p_names[] = {"compartmentOne", "I", "k__1", "k__2", "k__3", "k__4", "k__5", "k__6", "k__7", "k__8", "k__9", "k__10", "k__11", "k__12",  "" };
const char* x_names[] = {"SimData__1", "SimData__2", "SimData__3",  "" };
const char* y_names[] = {"t",  "" };
const char* xc_names[] = {"SimData__1", "SimData__2", "SimData__3",  "" };
const char* pc_names[] = { "" };
const char* yc_names[] = { "" };
const char* dx_names[] = {"ODE SimData__1", "ODE SimData__2", "ODE SimData__3",  "" };
const char* ct_names[] = { "" };
#endif // NAME_ARRAYS

#ifdef INITIAL
x[0] = 1;	//metabolite 'SimData__1': ode
x[1] = 1;	//metabolite 'SimData__2': ode
x[2] = 1;	//metabolite 'SimData__3': ode
#endif /* INITIAL */

#ifdef FIXED
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
y[0] = T;	//model entity 't':assignment
x_c[0] = x[0]/p[0];	//concentration of metabolite 'SimData__1': ode
x_c[1] = x[1]/p[0];	//concentration of metabolite 'SimData__2': ode
x_c[2] = x[2]/p[0];	//concentration of metabolite 'SimData__3': ode
#endif /* ASSIGNMENT */

#ifdef FUNCTIONS_HEADERS
#endif /* FUNCTIONS_HEADERS */

#ifdef FUNCTIONS
#endif /* FUNCTIONS */

#ifdef ODEs
dx[0] = p[2]*p[1]*(1.00000000000000000-x_c[0])/(1.00000000000000000-x_c[0]+p[8])-p[3]*x_c[0]/(x_c[0]+p[9]) * p[0];	//model entity 'SimData__1':ode
dx[0] = p[2]*p[1]*(1.00000000000000000-x_c[0])/(1.00000000000000000-x_c[0]+p[8])-p[3]*x_c[0]/(x_c[0]+p[9]) * p[0];	//model entity 'SimData__1':ode
dx[1] = p[4]*x_c[0]*(1.00000000000000000-x_c[1])/(1.00000000000000000-x_c[1]+p[10])-p[5]*x_c[1]/(x_c[1]+p[11]) * p[0];	//model entity 'SimData__2':ode
dx[1] = p[4]*x_c[0]*(1.00000000000000000-x_c[1])/(1.00000000000000000-x_c[1]+p[10])-p[5]*x_c[1]/(x_c[1]+p[11]) * p[0];	//model entity 'SimData__2':ode
dx[2] = p[6]*x_c[0]*(1.00000000000000000-x_c[2])/(1.00000000000000000-x_c[2]+p[12])-p[7]*x_c[1]*x_c[2]/(x_c[2]+p[13]) * p[0];	//model entity 'SimData__3':ode
dx[2] = p[6]*x_c[0]*(1.00000000000000000-x_c[2])/(1.00000000000000000-x_c[2]+p[12])-p[7]*x_c[1]*x_c[2]/(x_c[2]+p[13]) * p[0];	//model entity 'SimData__3':ode
#endif /* ODEs */
