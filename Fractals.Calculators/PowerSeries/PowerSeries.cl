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
							global result* outputBuffer
						  )
{
	// some variables we might need
	result myresult;
	double2 sum;
	sum.x = 0;
	sum.y = 0;
	
	double temp;

	double2 z = GetPosition(antiAliasingSamples, size, viewPortSize, viewPortDefaultOffset, rotationInfo, zoomFactor, zoomOffset);	

	// write z already into result 
	myresult.cx = z.x;
	myresult.cy = z.y;
	
	// * z-z0 *
	z.x = z.x - 3;
	z.y = z.y - 0;

	double2 zDiff;
	zDiff.x = 1;
	zDiff.y = 0;

	int n = 1;
	
	double an;

  	while( (n < maxIterations) && ((sum.x*sum.x+sum.y*sum.y) < escapeOrbitSquared) )
	{
		// (z-z0)^n = (z-z0)^(n-1) * (z-z0)
		//for (int i = 0; i < 3; i++) {
			temp = zDiff.x * z.y + zDiff.y * z.x;
			zDiff.x = zDiff.x * z.x - zDiff.y*z.y;
			zDiff.y = temp;		
		//}

        an = pow(-1,(double)(n))/(double)n;

		sum.x = sum.x + an * zDiff.x;
		sum.y = sum.y + an * zDiff.y;

		n++;
	}
		
    // write the results to output array
	myresult.zx = z.x;
	myresult.zy = z.y;
	
	// just use the iteration count
	myresult.n = n;

	outputBuffer[get_global_id(0) - offset] = myresult;
}
