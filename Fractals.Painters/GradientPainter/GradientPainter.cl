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

uchar4 Paint(double data, const uchar4 color1, const uchar4 color2)
{
	uchar4 result;

	result.x = (uchar)(color1.x + (color2.x - color1.x) * data);
	result.y = (uchar)(color1.y + (color2.y - color1.y) * data);
	result.z = (uchar)(color1.z + (color2.z - color1.z) * data);
	result.w = (uchar)(color1.w + (color2.w - color1.w) * data);

	return result;
}
