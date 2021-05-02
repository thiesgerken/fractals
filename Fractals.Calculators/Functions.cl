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

#pragma OPENCL EXTENSION cl_khr_global_int32_base_atomics : enable

/* structure to store iteration results in */
typedef struct { double cx, cy, zx, zy, n; } result;

// helper functions 
double2 ScreenToViewport(int2 coordinate, int2 size, double scale, double2 defaultOffset, double2 zoomOffset, double2 rotation)
{
	double2 result;

	result.x = (coordinate.x - size.x /2)*scale;
	result.y = (-coordinate.y + size.y / 2) * scale;

	// apply rotation
	return (double2)(result.x*rotation.y-result.y*rotation.x,result.x*rotation.x+result.y*rotation.y) + defaultOffset + zoomOffset;
}

double2 GetPosition(int antiAliasingSamples, int2 size, float2 viewPortSize, double2 viewPortDefaultOffset, double2 rotationInfo, double zoomFactor, double2 zoomOffset)
{
	// calculate scaling     
	double scale = max(viewPortSize.x / size.x , viewPortSize.y / size.y) / zoomFactor; 
 
	// get position from id
	int samplesRoot = (int)sqrt((float)antiAliasingSamples);
	int x = get_global_id(0) % (int)(size.x*antiAliasingSamples);
	int2 screen;
	screen.y = (get_global_id(0) - x)/(size.x*antiAliasingSamples);
	screen.x = x / antiAliasingSamples;

	int subId =  x % antiAliasingSamples;
	
	int2 sub;
	sub.x = subId % samplesRoot;
	sub.y = (subId - sub.x) / samplesRoot;

	double pixelWidth  = length(ScreenToViewport((int2)(1, 1), size, scale, viewPortDefaultOffset, zoomOffset, rotationInfo) - ScreenToViewport((int2)(2, 1), size, scale, viewPortDefaultOffset, zoomOffset, rotationInfo));
	double pixelHeight = length(ScreenToViewport((int2)(1, 1), size, scale, viewPortDefaultOffset, zoomOffset, rotationInfo) - ScreenToViewport((int2)(1, 2), size, scale, viewPortDefaultOffset, zoomOffset, rotationInfo));

	double2 center = ScreenToViewport(screen, size, scale, viewPortDefaultOffset, zoomOffset, rotationInfo);
	
	return (double2)(center.x - pixelWidth/2 + (sub.x + 0.5) * pixelWidth / samplesRoot, center.y - pixelHeight/2 + (sub.y + 0.5) * pixelHeight / samplesRoot);
}

/*
Signature that calculators have to use
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
							global result* outputBuffer, // outputBuffer[id - offset] for current sample
							** calculator-specific params, to be assigned in SetArguments(), do not have to be registered **
						  ) 
*/

/*
Signature that painters have to use
char4 Paint(
			double data
			**painter-specific params, to be assigned in SetArguments(), Have to be registered!**
			)
*/

/*
Signature that datasources have to use
double GetData(
			   int maxIterations,
			   result iterResult
			   ** datasource-specific params, to be assigned in SetArguments(), Have to be registered! **
			   )
*/
