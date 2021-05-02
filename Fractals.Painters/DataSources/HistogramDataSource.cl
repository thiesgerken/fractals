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

double GetData(int maxIterations, result iterResult, global int* cdf, const int count, const int res)
{
	return (double)cdf[(int)(iterResult.n * res)] / count;
}

kernel void CreateHistogram(const int res, global result* results, global volatile int* histogram)
{
	atom_inc(&histogram[(int)(res*results[get_global_id(0)].n)]);
}
