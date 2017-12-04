#include <iostream>
#include <iomanip> // more decimal precision
#include <fstream> // file stream
#include <sstream> // string stream
#include <cmath>
#include <ctime> // calculate time between dates
#include <time.h> // time code
#include <vector>

using namespace std;

const double p = 0.5; // global variable
const double q = 1 - p; // global variable


vector<string> splitstr(string str, char c) { // split string into vector by commas
	vector<string> out;
	stringstream ss(str);
	
	while (ss.good()) {
		string substr;
		getline(ss, substr, c);
		out.push_back(substr);
	}
	return out;
}
tm make_tm(string date) { // converts string to date/time struct tm
	// assumes YYYYMMDD date format
	int year, month, day;
	year = stoi( date.substr(0, 4) );
	month = stoi( date.substr(4, 2) );
	day = stoi( date.substr(6, 2) );
    tm tm = {0};
    tm.tm_year = year - 1900; // years count from 1900
    tm.tm_mon = month - 1;    // months count from Jan = 0
    tm.tm_mday = day;         // days count from 1
    return tm;
}
double u_fxn(double R, double sigma, double dt) { // up factor function
	return exp(sigma * sqrt(dt) + (R - 0.5 * pow(sigma, 2)) * dt);
}
double d_fxn(double R, double sigma, double dt) { // down factor function
	return exp(-sigma * sqrt(dt) + (R - 0.5 * pow(sigma, 2)) * dt);
}
double price_asset(double S0, int n, int h, double u, double d) { // asset price function
	return S0 * pow(u, h) * pow(d, n - h); // we should probably check if 0 <= h <= n...
}
double payoff(double S, double K, char type) {
	if (type == 'c')  // payoff if call option
		return fmax(S - K, 0); 
	else if (type == 'p') // payoff if put option
		return fmax(K - S, 0);
	else 
		return -1; // do something obviously wrong if input is invalid
}
double price_option_r(int n, int h, int N, double S0, double K, 
	double R, double sigma, double dt, char type) { // naive recursive implementation
	
	if (n == N) { // terminal node is simply the payoff
		double u = u_fxn(R, sigma, dt);
		double d = d_fxn(R, sigma, dt);
		double S = price_asset(S0, n, h, u, d);
		return payoff(S, K, type);
	}
	else { // if we're not at the terminal node, use the recursive algorithm
		return pow(1 + R, -dt) * ( p * price_option_r(n + 1, h + 1, N, S0, K, R, sigma, dt, type)
			+ q * price_option_r(n + 1, h, N, S0, K, R, sigma, dt, type) );
	}
}
double price_option_i(int N, double S0, double K, 
	double R, double sigma, double dt, char type) { // iterative implementation
	// assumes you want the time-zero price of the option
	double prices[N + 1], S;
	double u = u_fxn(R, sigma, dt);
	double d = d_fxn(R, sigma, dt);
	
	// first loop over all the terminal nodes for all possible H and T combinations
	// to compute V_N = payoff 
	for (int i = 0; i < N + 1; i++) { // traverse upwards through the nodes
		S = price_asset(S0, N, i, u, d); 
		prices[i] = payoff(S, K, type);
	}
	// use the recursive algorithm to price nodes prior to the terminal nodes
	for (int i = N - 1; i >= 0; i--) // traverse backwards through the tree
		for (int j = 0; j < i + 1; j++) // traverse upwards through the nodes
			prices[j] = p * prices[j + 1] + q * prices[j];
	return pow(1 + R, -dt * N) * prices[0];
}
double implied_vol(double V_obs, double S0, double K, 
	int N, double R, double dt, char type, double eps = pow(10,-5)) { // bisection
	// bisection algorithm to solve for an implied volatility estimate
	const double ESCAPE_MIN = pow(10,-5); // value to quit if we go too low
	const double ESCAPE_MAX = 10; // value to quit if we go too high

	double V_test_lo, V_test_hi, V_test, sig_test;
	double sig_lo = 0.10; 
	double sig_hi = 0.50;
	
	do {
		V_test_lo = price_option_i(N, S0, K, R, sig_lo, dt, type);
		V_test_hi = price_option_i(N, S0, K, R, sig_hi, dt, type);
				
		if ( fabs(V_test_lo - V_obs) < eps) // first guess for sig_lo was correct
			return sig_lo;
		else if ( fabs(V_test_hi - V_obs) < eps ) // first guess for sig_hi was correct
			return sig_hi;			
		else if ( V_test_lo > V_obs ) { // sig_lo was originally set too high 
			sig_hi = sig_lo;
			sig_lo = sig_lo/10;
		} else if ( V_test_hi < V_obs ) { // sig_hi was originally set too low
			sig_lo = sig_hi;
			sig_hi = 2 * sig_hi;
		} else {  // sig_obs is somewhere in between sig_lo and sig_hi
			do {
				sig_test = (sig_lo + sig_hi)/2; // bisect!
				V_test = price_option_i(N, S0, K, R, sig_test, dt, type);
				
				if ( V_test > V_obs ) // price too high: contract right endpoint
					sig_hi = sig_test;
				else // price too low: contract left endpoint
					sig_lo = sig_test;
			} while ( fabs(V_test - V_obs) > eps );
			return sig_test;
		}
	} while ( (sig_lo > ESCAPE_MIN) & (sig_hi < ESCAPE_MAX) );
	return -1; // do something obviously wrong is we reach the escape values
}

int main() {
	string filepath = "google-opt2_old.csv";
 	double S0[] = {549.21, 553.95, 555.29};
 	int N[] = {pow(10,1),pow(10,2),pow(10,3),pow(10,4),pow(10,5)};
 	double R = 0.005;
 	double vol, dt; char type;
 	vector<double> taus, strikes, asks, spots;
 	vector<string> types;
 	 	
 	// READ DATA
 	/*
 		Our approach will be to loop over each line in the CSV and save each column
 		to its own vector. Later we will loop over each vector value to compute
 		the relevant implied vols.
 	*/
	ifstream data (filepath);
	string line; // each row of the csv prior to processing
	vector<string> row; // we will separate the csv rows into their entries
	struct tm tm1, tm2; // to store the issue date & expiry
	
	getline(data, line); // read header	
	while (getline(data, line)) { // loop over each line in the CSV
 		row = splitstr(line, ','); // split line on comma
 		 
 		// get start & expiry dates for years to expiry 		
 		tm1 = make_tm( row.at(0) );
 		tm2 = make_tm( row.at(1) );
 
		taus.push_back( difftime(mktime(&tm2), mktime(&tm1))/(60 * 60 * 24 * 365) );
 		types.push_back( row.at(2) ); // "C" call or "P" put
 		strikes.push_back( stod(row.at(3)) );
 		asks.push_back( stod(row.at(4)) );
		
  		if (tm1.tm_mday == 1) { // select appropriate closing price
  			spots.push_back( S0[0] );
  		} else if (tm1.tm_mday == 2) {
  			spots.push_back( S0[1] );
  		} else if (tm1.tm_mday == 3) {
  			spots.push_back( S0[2] );
  		}
	}
  	data.close(); // close input csv

  	// WRITE DATA
  	ofstream outfile;
  	outfile.open("implied_vols_googl_rounded.csv"); // initialized csv
  	outfile << "N,spots,strike,tau,type,ask,vol,time" << endl; // header
  	  	
  	for (int j = 0; j < (sizeof(N)/sizeof(N[0])); j++) { // loop over all values of N
  		clock_t t1, t2; // time code
  		
  		// COMPUTE IMPLIED VOLS  	  	
  		for (int i = 0; i < taus.size(); i++) { // loop over every option in the data set
  			dt = taus.at(i) / N[j];
  			
  			// check if call or put option
  			if ( types.at(i) == "C" ) {	
  				type = 'c';
 			} else if ( types.at(i) == "P" ){
 		 		type = 'p';
 			} else { // do something obviously wrong if input is unexpected
 				type = 'z';
 			}
 		
 			t1 = clock(); // start timer
 			// compute implied vol here
 			vol = implied_vol(asks.at(i), spots.at(i), strikes.at(i), N[j], R, dt, type);
  			t2 = clock(); // end timer
  			float diff = ((float)t2 - (float)t1); // compute time
  			
  			// print to CSV
  			outfile << N[j] << "," << spots.at(i) << "," << strikes.at(i) 
  			<< "," << taus.at(i) << "," << types.at(i) << "," << asks.at(i) 
  			<< "," << vol << "," << diff/CLOCKS_PER_SEC << endl;
  			
  			// print to console
			cout << i << "\t" << "N: 10^" << log10(N[j]) << "\t vol: " << vol 
			<< "\t time: " << diff/CLOCKS_PER_SEC << endl;
  		}
		cout << "---------------------------------------------------" << endl;
  	}
  	
  	outfile.close();
}





































	