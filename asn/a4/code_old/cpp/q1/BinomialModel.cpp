#include "BinomialModel.h"

double BinomialModel::p = 0.5;
double BinomialModel::u = 1;
double BinomialModel::d = 1;

double BinomialModel::assetPrice(Asset A, double tau, int n, int N) {
	// compute future asset price at time tau given
	// n up steps in a N steps in a binomial tree in tau time
	double sigma = A.getSigma();
	double R = A.getR();
	double dt = tau/N; // time step interval
	
	// compute up & down factors
	u = exp(sigma * sqrt(dt) + (R - 0.5 * pow(sigma, 2)) * dt);
	d = exp(-sigma * sqrt(dt) + (R - 0.5 * pow(sigma, 2)) * dt);
	
	// Sn = S0 * u^n * d^(N-n)
	double S_N = A.getSpot() * pow(u, n) * pow(d, N - n);
	return S_N;
} 
double BinomialModel::optionValuation(Option* O, int N) {
	// compute option value at time 0 given N steps in a binomial tree
	
	// we rely heavily on pointers since Option O will either contain a
	// CallOption or a PutOption and we wish to use their function implementations
	// so that the code may remain general as possible 
	// (also, I'm also not very good at C++)
	Asset A = O->getAsset();
	double R = A.getR();
	double tau = O->getTau();
	double strike = O->getStrike();
	double dt = tau/N;

	double S, V;	
		
	double prices[N + 1];
	for (int k = 0; k <= N; k++) {
		// compute asset price at time N give k up steps
		S = assetPrice(A, tau, k, N); 
		prices[k] = O->payoff(S);
	}
	
	for (int k = N - 1; k >= 0; k--) {
		for (int l = 0; l <= k; l++) {
			prices[l] = 1/pow(1 + R, dt) * (p * prices[l + 1] + (1 - p) * prices[l]);
		}
	} 
	return prices[0];	
}

double BinomialModel::impliedVol(double V_obs, Option* O, int N, double eps) {
	// bisection algorithm to back out implied volatility estimate
	// given observed market prices V_obs, Option O, N steps in a binomial model
	// and some tolerance parameter eps
	
	Asset A = O->getAsset();
	double spot = A.getSpot();
	double R = A.getR();
	double tau = O->getTau();
	double dt = tau/N;
	
	double V_test_lo, V_test_hi, V_test, sig_test;
	double sig_lo = 0.01;
	double sig_hi = 1;
	
	do {
		O->setAsset(Asset(spot, R, sig_lo));
		V_test_lo = optionValuation(O, N);
		O->setAsset(Asset(spot, R, sig_hi));
		V_test_hi = optionValuation(O, N);
		
		if ( fabs(V_test_lo - V_obs) < eps ) {
			return sig_lo;
		} else if ( fabs(V_test_hi - V_obs) < eps ) {
			return sig_hi;
		} else if ( (V_test_lo < V_obs) && (V_test_hi > V_obs) ) { 
			// V_obs is somewhere in between
			sig_test = (sig_lo + sig_hi)/2;
			O->setAsset(Asset(spot, R, sig_test));
			V_test = optionValuation(O, N);
			
			while ( fabs(V_test - V_obs) > eps ) {
				if ( V_test > V_obs ) { // price too high: contract right endpoint
					sig_hi = sig_test;
				} else { // price too low: contract left endpoint
					sig_lo = sig_test;
				}
				sig_test = (sig_lo + sig_hi)/2;
				O->setAsset(Asset(spot, R, sig_test));
				V_test = optionValuation(O, N);
			}
			
		} else if ( V_test_lo > V_obs ) { // sig_lo was originally set too high 
			sig_lo = sig_lo/10;
			if ( sig_lo < eps ) { // give up if we go down too low
				return 0; 
			}
		} else { // sig_hi was originally set too high
			sig_hi = 2 * sig_hi; // should we set an escape value if we get too high?
		}
		
		return sig_test; // return sig_test from earlier nested while loop 
	
	} while ( (sig_lo > pow(10,-6)) || (sig_hi < 10) );
}

















