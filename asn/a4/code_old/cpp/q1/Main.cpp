#include <iostream>
#include <fstream>
#include <cmath>
#include <ctime> // calculate time between dates
#include <time.h> // time code
#include <string> // getline
#include <sstream> 
#include <vector>
#include "Asset.h"
#include "BinomialModel.h"
#include "CallOption.h"
#include "PutOption.h"

using namespace std;

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
tm make_tm(string date) { // assumes YYYYMMDD date format
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

int main() {
	double ask = 10;
	double S0 = 100;
	double strike = 100;
	int N = pow(10,5);
	double R = 0.05;
	double sigma = 0.25;
	double tau = 1;
	
	Asset A(S0, R, sigma);
	CallOption C(A, strike, tau);
	CallOption* CO = &C;

	clock_t t1, t2;
	t1 = clock();
	cout << BinomialModel::impliedVol(ask, CO, N) << endl;
	//cout << BinomialModel::optionValuation(CO, N) << endl;
	//std::cout << BinomialModel::assetPrice(A, tau, 5, N) << std::endl;
	t2 = clock();
	float diff ((float) t2 - (float) t1);
	cout << diff/CLOCKS_PER_SEC << endl;

	string filepath = "../../../data/in/google-opt2_old.csv";
 	//double S0[] = {549.21, 553.95, 555.29};
 	//int N[] = {pow(10,1),pow(10,2),pow(10,3),pow(10,4),pow(10,5)};
 	//double R = 0.005;
 	double vol;
 	vector<double> taus, strikes, asks, spots;
 	vector<string> types;
 	
 	
 	// READ DATA
	ifstream data (filepath);
	string line; // individual row/line of the csv
	vector<string> row; // row split by elements delimited by commas
	struct tm tm1, tm2; // store date & expiry
	
	getline(data, line); // read header	
	while (getline(data, line)) {
 		row = splitstr(line, ',');
 		 
 		// get start & expiry dates for years to expiry 		
 		tm1 = make_tm( row.at(0) );
 		tm2 = make_tm( row.at(1) );
 
		taus.push_back( difftime(mktime(&tm2), mktime(&tm1))/(60 * 60 * 24 * 365) );
 		types.push_back( row.at(2) ); // "C" call or "P" put
 		strikes.push_back( stod(row.at(3)) );
 		asks.push_back( stod(row.at(4)) );
		
//  		if (tm1.tm_mday == 1) { // select appropriate closing price
//  			spots.push_back( S0[0] );
//  		} else if (tm1.tm_mday == 2) {
//  			spots.push_back( S0[1] );
//  		} else if (tm1.tm_mday == 3) {
//  			spots.push_back( S0[2] );
//  		}
	}
  	data.close(); // close input csv
  	
  	
  	
  	
  	
  	// WRITE DATA
//   	ofstream outfile;
//   	outfile.open("../../../data/out/implied_vols.csv"); // initialized csv
//   	outfile << "N,spots,strike,tau,type,ask,vol" << endl; // header
//   	  	
//   	for (int j = 0; j < (sizeof(N)/sizeof(N[0])); j++) {
//   		clock_t t1, t2;
//   		t1 = clock();
//   		
//   		// COMPUTE IMPLIED VOLS  	  	
//   		for (int i = 0; i < taus.size(); i++) {
//   			Asset A(spots.at(i), R);
//   		
//   			if (types.at(i) == "C") {	
//   				CallOption C(A, strikes.at(i), taus.at(i));
//   				CallOption* CO = &C;
//  				vol = BinomialModel::impliedVol(asks.at(i), CO, N[j]);
//  			} else {
//  		 		PutOption P(A, strikes.at(i), taus.at(i));
//   				PutOption* PO = &P;
//  				vol = BinomialModel::impliedVol(asks.at(i), PO, N[j]);
//  			}
//   			
//   			outfile << N[j] << "," << spots.at(i) << "," << strikes.at(i) 
//   			<< "," << taus.at(i) << "," << types.at(i) << "," << asks.at(i) 
//   			<< "," << vol << endl;
//   			
// 			cout << "10^" << log10(N[j]) << "\t\t" << i << endl;
//   		}
//   		t2 = clock();
//   		float diff ((float)t2 - (float)t1);
// 		cout << "10^" << log10(N[j]) << "\t\t" << diff/CLOCKS_PER_SEC << endl;
//   	}
//   	
//   	outfile.close();
  	
}


	