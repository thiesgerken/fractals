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

Imports System.Drawing

''' <summary>
''' Represents a 2D Vector of type Double.
''' </summary>
Public Class Vector2
    Inherits Matrix

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the first vector component.
    ''' </summary>
    ''' <value>The X.</value>
    Public Property X As Double
        Get
            Return Me(0, 0)
        End Get
        Set(ByVal value As Double)
            Me(0, 0) = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the second vector component.
    ''' </summary>
    ''' <value>The Y.</value>
    Public Property Y As Double
        Get
            Return Me(0, 1)
        End Get
        Set(ByVal value As Double)
            Me(0, 1) = value
        End Set
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Vector2" /> class.
    ''' </summary>
    ''' <param name="x">The initial x value.</param>
    ''' <param name="y">The initial y value.</param>
    Public Sub New(ByVal x As Double, ByVal y As Double)
        MyBase.New(1, 2)
        Me.X = x
        Me.Y = y
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Vector2" /> class.
    ''' </summary>
    Public Sub New()
        Me.New(0, 0)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Vector2" /> class.
    ''' </summary>
    ''' <param name="m">The matrix to take initial values from</param>
    Public Sub New(ByVal m As Matrix)
        Me.New(m(0, 0), m(0, 1))
    End Sub

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="p">The p to convert to a vector</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Widening Operator CType(ByVal p As PointF) As Vector2
        Return New Vector2(p.X, p.Y)
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Widening Operator CType(ByVal p As Point) As Vector2
        Return New Vector2(p.X, p.Y)
    End Operator

    ''' <summary>
    ''' Implements the operator +.
    ''' </summary>
    ''' <param name="vecA">The vec A.</param>
    ''' <param name="vecB">The vec B.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator +(ByVal vecA As Vector2, ByVal vecB As Vector2) As Vector2
        Return New Vector2(vecA.ToMatrix + vecB.ToMatrix)
    End Operator

    ''' <summary>
    ''' Implements the operator -.
    ''' </summary>
    ''' <param name="vecA">The vec A.</param>
    ''' <param name="vecB">The vec B.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator -(ByVal vecA As Vector2, ByVal vecB As Vector2) As Vector2
        Return New Vector2(vecA.ToMatrix - vecB.ToMatrix)
    End Operator

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="vec">The vec.</param>
    ''' <param name="r">The r.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator *(ByVal vec As Vector2, ByVal r As Double) As Vector2
        Return New Vector2(vec.ToMatrix * r)
    End Operator

    ''' <summary>
    ''' Implements the operator /.
    ''' </summary>
    ''' <param name="vec">The vec.</param>
    ''' <param name="r">The r.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator /(ByVal vec As Vector2, ByVal r As Double) As Vector2
        Return New Vector2(vec.ToMatrix * (1 / r))
    End Operator

    ''' <summary>
    ''' Dot product of 2 vectors
    ''' </summary>
    ''' <param name="vecA">The vec A.</param>
    ''' <param name="vecB">The vec B.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator *(ByVal vecA As Vector2, ByVal vecB As Vector2) As Double
        Return vecA.X * vecB.X + vecA.Y * vecB.Y
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="v">The v.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Narrowing Operator CType(ByVal v As Vector2) As Point
        Return New Point(CInt(v.X), CInt(v.Y))
    End Operator

#End Region

#Region "Methods"

    ''' <summary>
    ''' Toes the matrix.
    ''' </summary>
    ''' <returns></returns>
    Public Function ToMatrix() As Matrix
        Return ToMatrix(Me)
    End Function

    ''' <summary>
    ''' Gets the unit vector of this vector (the vector with length=1 and same direction)
    ''' </summary>
    ''' <returns></returns>
    Public Function GetUnitVector() As Vector2
        Return Me / Me.Abs
    End Function

    ''' <summary>
    ''' Converts a given vector to a matrix
    ''' </summary>
    ''' <param name="vec">The vector to convert.</param>
    ''' <returns></returns>
    Public Shared Function ToMatrix(ByVal vec As Vector2) As Matrix
        Dim m As New Matrix(1, 2)

        m(0, 0) = vec.X
        m(0, 1) = vec.Y

        Return m
    End Function

    ''' <summary>
    ''' Gets the length of this vector
    ''' </summary>
    ''' <returns></returns>
    Public Function Abs() As Double
        Return Math.Sqrt(X * X + Y * Y)
    End Function

#End Region
End Class
