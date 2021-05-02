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

kernel void PaintPixel(const int maxIterations, 
					   const int antiAliasingSamples,
					   global result* resultBuffer,
					   global uchar* bitmapBuffer
					   /*{declarations}*/
					  )
{
	int r = 0;
	int g = 0;
	int b = 0;
			
	int id = get_global_id(0);

	for (int i = antiAliasingSamples * id; i < antiAliasingSamples * (id + 1); i++)
	{           
		if (resultBuffer[i].n == maxIterations)
		{   
			// InsidePainter   
			double data = GetData_Inside(maxIterations, resultBuffer[i] /*{params_inside_data}*/);
			uchar4 color = Paint_Inside(data /*{params_inside_painter}*/);
	
			r += color.x;
			g += color.y;
			b += color.z;
		}
		else
		{
			// OutsidePainter
			double data = GetData_Outside(maxIterations, resultBuffer[i] /*{params_outside_data}*/);
			uchar4 color = Paint_Outside(data /*{params_outside_painter}*/);

			r += color.x;
			g += color.y;
			b += color.z;
		}
	}

	bitmapBuffer[4 * id + 0] = b / antiAliasingSamples;
	bitmapBuffer[4 * id + 1] = g / antiAliasingSamples;
	bitmapBuffer[4 * id + 2] = r / antiAliasingSamples;
}
