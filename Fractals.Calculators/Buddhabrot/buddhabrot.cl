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

/* Buddhabrot OpenCL Kernels
   parts of the code by ker2x http://github.com/ker2x/WinBuddhaOpenCL */

// Check if chosen point is in MSet
bool isInMSet(float2 c,
			  const int maxIter,
			  const float escapeOrbit)
{
    float cy2 = c.y*c.y;
   
    // Quick rejection check if c is in 2nd order period bulb
    if( (c.x+1.0) * (c.x+1.0) + cy2 < 0.0625) return true;

    // Quick rejection check if c is in main cardioid
    float q = (c.x-0.25)*(c.x-0.25) + cy2;
    if( q*(q+(c.x-0.25)) < 0.25*cy2) return true; 

    // test for the smaller bulb left of the period-2 bulb
    if (( ((c.x+1.309)*(c.x+1.309)) + cy2) < 0.00345) return true;

    // check for the smaller bulbs on top and bottom of the cardioid
    if ((((c.x+0.125)*(c.x+0.125)) + (c.y-0.744)*(c.y-0.744)) < 0.0088) return true;
    if ((((c.x+0.125)*(c.x+0.125)) + (c.y+0.744)*(c.y+0.744)) < 0.0088) return true;

	// check by iteration
    int iter = 0;
    float2 z = {0,0};
    float temp;

    while( (iter < maxIter) && ((z.x*z.x+z.y*z.y) < escapeOrbit) )
    {
       temp = 2 * z.x * z.y;
       z.x = z.x*z.x - z.y*z.y + c.x;
       z.y = temp + c.y;
	
	   iter++;
    }

	return iter == maxIter;
}

// Main kernel
kernel void buddhabrot(const int minIter,				 
					   const int maxIter,
					   const int2 size,
					   const float2 vpSize,
					   const float escapeOrbit,
					   const float2 offset,
					   const float2 rotationAngle, // .X = sin(alpha), .Y = cos(alpha)
					   const int options, // smooth 
					   global float2* randomXYBuffer,
					   global uint* outputBuffer)
{
	// region to take samples from
	const float realMin = -2.01;
    const float realMax = 0.5;
    const float imaginaryMin = -1.2;
    const float imaginaryMax = 1.2; 
	
	// random number
    float2 rand = randomXYBuffer[get_global_id(0)];    

	// pick a sample using the random number
    float2 c;
    c.x = mix(realMin, realMax, rand.x);
    c.y = mix(imaginaryMin, imaginaryMax, rand.y);

	float temp = 0.0;
    int2 pos;
	float2 z = {0,0};

	int iter = 0;
    
    float scale = max(vpSize.x / size.x,  vpSize.y / size.y);

    // debug, show all points that were taken into account
	// pos.x = ((c.x - offset.x) * rotationAngle.y - (c.y - offset.y) * rotationAngle.x) / scale + width / 2;
	// pos.y = ((c.y - offset.y) * rotationAngle.y + (c.x - offset.x) * rotationAngle.x) / scale + height / 2;
	
	int off;

    if(! isInMSet(c, maxIter, escapeOrbit))
    {    
		while( (iter < maxIter) && ((z.x*z.x+z.y*z.y) < escapeOrbit) )
        {
            temp = 2 * z.x * z.y;
            z.x = z.x*z.x - z.y*z.y + c.x;
            z.y = temp + c.y;
			
			// translate to bitmap coordinates
			// (zr - offset.x) -> set zero point right
			// x * rotationAngle.y - (zi - offset.y) * rotationAngle.x -> rotate point
			// x / scale + width / 2 -> translate to bitmap size
			pos.x = ((z.x - offset.x) * rotationAngle.y - (z.y - offset.y) * rotationAngle.x) / scale + size.x / 2;
            pos.y = ((z.y - offset.y) * rotationAngle.y + (z.x - offset.x) * rotationAngle.x) / scale + size.y / 2;
 
            if( iter > minIter && pos.x >= 0 && pos.y >= 0 && pos.x < size.x && pos.y < size.y )
            {
					off = pos.x + (pos.y * size.x);	
					outputBuffer[off] += 150; 

					if (options == 1 && pos.x > 0 && pos.x < size.x - 1 && pos.y > 0 && pos.y < size.y - 1)
					{
						outputBuffer[off + size.x]  += 4; 
						outputBuffer[off - size.x]  += 4; 

						outputBuffer[off + 1 + size.x]++; 
						outputBuffer[off + 1] += 4; 
						outputBuffer[off + 1 - size.x]++; 

						outputBuffer[off - 1 + size.x]++; 
						outputBuffer[off - 1] += 4; 
						outputBuffer[off - 1 - size.x]++; 
					}				
            }

            iter++;
        }
    }
}

// Shift random numbers
kernel void xorshift(uint s1,
					 uint s2,
					 uint s3,
					 uint s4,
					 const int bufferSize,
					 global float2* randomXYBuffer)
{
    uint st;
    float2 tmp;

    for(int i=0; i < bufferSize; i++)
    {
        st = s1 ^ (s1 << 11);
        s1 = s2;
        s2 = s3;
        s3 = s4;
        s4 = s4 ^ (s4 >> 19) ^ ( st ^ (st >> 18));
        tmp.x = (float)s4 / UINT_MAX;

        st = s1 ^ (s1 << 11);
        s1 = s2;
        s2 = s3;
        s3 = s4;
        s4 = s4 ^ (s4 >> 19) ^ ( st ^ (st >> 18));
        tmp.y = (float)s4 / UINT_MAX;
        randomXYBuffer[i] = tmp;

    }
}
