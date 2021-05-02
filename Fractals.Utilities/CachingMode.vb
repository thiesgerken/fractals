' Copyright (C) 2010-2012 Thies Gerken

' This file is part of Fractals.

' Fractals is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.

' Fractals is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with Fractals. If not, see <http://www.gnu.org/licenses/>.

Public Enum CachingMode
    ''' <summary>
    ''' No Usage.
    ''' </summary>
    None = 0
    ''' <summary>
    ''' The cache contains data that can be reused.
    ''' </summary>
    Read = 1
    ''' <summary>
    ''' The cache does not contain relevant data; it needs to be calculated and can be written to the cache.
    ''' </summary>
    Write = 2
End Enum