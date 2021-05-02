// Copyright (C) 2010-2012 Thies Gerken

// This file is part of Fractals.

// Fractals is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Fractals is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Fractals. If not, see <http://www.gnu.org/licenses/>.

/* Perform a check on a point without having to iterate */
bool FastCheck(double2 c)
{
    double cy2 = c.y*c.y;
   
    // Quick rejection check if c is in 2nd order period bulb
    if( (c.x+1.0) * (c.x+1.0) + cy2 < 0.0625) return true;

    // Quick rejection check if c is in main cardioid
    double q = (c.x-0.25)*(c.x-0.25) + cy2;
    if( q*(q+(c.x-0.25)) < 0.25*cy2) return true; 
	
    // test for the smaller bulb left of the period-2 bulb
    if (( ((c.x+1.309)*(c.x+1.309)) + c.y*c.y) < 0.00345) return true;

    // check for the smaller bulbs on top and bottom of the cardioid
    if ((((c.x+0.125)*(c.x+0.125)) + (c.y-0.744)*(c.y-0.744)) < 0.0088) return true;
    if ((((c.x+0.125)*(c.x+0.125)) + (c.y+0.744)*(c.y+0.744)) < 0.0088) return true;

    return false;
}

// Main Kernel
kernel void CalculateSample(
							const int2 size,
							const float2 viewPortSize,
							const double2 viewPortDefaultOffset,
							const int2 options, // needsInsideOrbit, smooth
							const int maxIterations, 
							const float escapeOrbitSquared, 
							const int antiAliasingSamples,
							const double zoomFactor,
							const double2 zoomOffset,
							const double2 rotationInfo, // .x = sin(alpha) .y = cos(alpha)
							const int offset,
							global result* outputBuffer,
							const double k,
							const int invert
						  )
{
	// some variables we might need
	result myresult;
	double2 z;
	z.x = 0;
	z.y = 0;
	int n = 0;
	double temp = 0;

	double2 c = GetPosition(antiAliasingSamples, size, viewPortSize, viewPortDefaultOffset, rotationInfo, zoomFactor, zoomOffset);	

	// write c already into result since it may be altered during inversion
	myresult.cx = c.x;
	myresult.cy = c.y;
	
    // should I invert the picture? (+ 1/c instead of +c)
	if (invert == 1)
	{
		temp = c.x*c.x+c.y*c.y;

		if (temp != 0) // dividing by zero is evil
		{
			c.x = c.x / temp;
			c.y = -c.y / temp;
		}
	}	
   
	if (options.x == 0 && k == 2)
	{
		if (FastCheck(c))
		{
			n = maxIterations;
		}
	}
	
	if (n==0) // fast check successful?
	{
		if (k == 2) // try to calculate the standard set as fast as possible
		{		
			while( (n < maxIterations) && ((z.x*z.x+z.y*z.y) < escapeOrbitSquared) )
			{
				temp = 2 * z.x * z.y + c.y;
			    z.x = z.x*z.x - z.y*z.y + c.x;
				z.y = temp;
				n++;
			}
		}
		else if (k == trunc(k)) // integer exponent other than 2
		{
			double2 oldz;
			int intk = (int)k;

			while( (n < maxIterations) && ((z.x*z.x+z.y*z.y) < escapeOrbitSquared) )
			{
				if (intk < 0)
				{
					//invert z
					temp = z.x*z.x+z.y*z.y;

					if (temp != 0) // dividing by zero is evil
					{
						z.x = z.x / temp;
						z.y = -z.y / temp;
					}
				}

				if (abs(intk) != 1)
				{
					oldz = z;
					
					for (int i=0; i<=abs(intk)-2; i++)
					{
						temp = z.x * oldz.y + z.y * oldz.x;
						z.x = z.x*oldz.x - z.y*oldz.y;
						z.y = temp;		
					}
				}

				z.x = z.x + c.x;
				z.y = z.y + c.y;

				n++;
			}
		}
		else // k is a float
		{
			double2 ln; 
			double ex;

			while( (n < maxIterations) && ((z.x*z.x+z.y*z.y) < escapeOrbitSquared) )
			{
				// raise z by k
				// 0^k = 0
				if (!(z.x == 0 && z.y == 0)) 
				{
					// z^k = e^(k*ln(z))
					// e^(x+yi) = e^x * e^(yi) = e^x * (cos(y) + sin(y) * i)
					// ln(z) = |z| + phi*i 

				    ln.x = sqrt(z.x*z.x+z.y*z.y);
					ln.y = acos(z.x / ln.x);
					ln.x = log(ln.x);

					if(z.y < 0)
					{
						ln.y = -ln.y;
					}

					if(ln.y < 0)
					{
						ln.y = ln.y + M_PI * 2;
					}

					ln.x = k * ln.x;
					ln.y = k * ln.y;
					
					ex = exp(ln.x);

					z.x = ex * cos(ln.y);
					z.y = ex * sin(ln.y);
				}

				z.x = z.x + c.x;
				z.y = z.y + c.y;

				n++;
			}
		}
	}

    // write the results to output array
	myresult.zx = z.x;
	myresult.zy = z.y;
	
	// smooth if desired
	if (options.y == 1 && n < maxIterations)
	{
		  // mu(z) = n - log_k (log|z_n|/log(bailout))
          myresult.n = n - log10(log10(length(z))/log10(sqrt(escapeOrbitSquared))) / log10(fabs(k));
	}
	else
	{
		// just use the iteration count
		myresult.n = n;
	}

	outputBuffer[get_global_id(0) - offset] = myresult;
}
