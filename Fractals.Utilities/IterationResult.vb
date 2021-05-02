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

Imports Fractals.Mathematics
Imports System.IO

''' <summary>
''' The Result of an Iteration
''' </summary>
Public Structure IterationResult
    Implements IBinarySaveable

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the position of the pixel in the complex plane.
    ''' </summary>
    ''' <value>The position.</value>
    Public Property Position As ComplexNumber

    ''' <summary>
    ''' Gets or sets the last value in the orbit.
    ''' </summary>
    ''' <value>The last value in the orbit.</value>
    Public Property Z As ComplexNumber

    ''' <summary>
    ''' Gets or sets the iteration count that is needed to make the pixel diverge/converge. if n = _maxN then the pixel does not diverge/converge.
    ''' </summary>
    ''' <value>The iteration count.</value>
    Public Property IterationCount As Double

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="IterationResult" /> class.
    ''' </summary>
    ''' <param name="iterationCount">The count of iterations needed to get the pixel to diverge/converge.</param>
    ''' <param name="z">The last number of the orbit.</param>
   ''' <param name="pos">The position.</param>
    Public Sub New(ByVal pos As ComplexNumber, ByVal z As ComplexNumber, ByVal iterationCount As Double)
        Me.Position = pos
        Me.Z = z
        Me.IterationCount = iterationCount
    End Sub

#End Region

#Region "IBinarySaveable Support"

    ''' <summary>
    ''' Returns an instance with data from a binary reader.
    ''' </summary>
    ''' <param name="reader">The binary reader.</param>
    Public Sub Load(ByVal reader As BinaryReader) Implements IBinarySaveable.Load
        Position = reader.ReadComplex()
        Z = reader.ReadComplex()
        IterationCount = reader.ReadDouble()
    End Sub

    ''' <summary>
    ''' Saves this instance to a binary writer.
    ''' </summary>
    ''' <param name="writer">The writer to write on.</param>
    Public Sub Save(ByVal writer As BinaryWriter) Implements IBinarySaveable.Save
        writer.Write(Position)
        writer.Write(Z)
        writer.Write(IterationCount)
    End Sub

#End Region
End Structure
