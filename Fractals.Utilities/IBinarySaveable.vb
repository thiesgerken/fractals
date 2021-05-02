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

Imports System.IO

''' <summary>
''' Interface for a class that can be loaded and saved from a binary stream.
''' </summary>
Public Interface IBinarySaveable

    ''' <summary>
    ''' Saves this instance to a binary writer.
    ''' </summary>
    ''' <param name="writer">The writer to write on.</param>
    Sub Save(ByVal writer As BinaryWriter)

    ''' <summary>
    ''' Returns an instance with data from a binary reader.
    ''' </summary>
    ''' <param name="reader">The binary reader.</param>
    Sub Load(ByVal reader As BinaryReader)
End Interface
