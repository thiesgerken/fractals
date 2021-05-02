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

Imports System

''' <summary>
''' Implements a 2D rotation matrix
''' </summary>
Public Class RotationMatrix
    Inherits Matrix

#Region "Fields"

    Private _angle As Double

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets the angle in radians.
    ''' </summary>
    ''' <value>The angle.</value>
    Public Property Angle As Double
        Get
            Return _angle
        End Get
        Set(ByVal value As Double)
            _angle = value
            Rebuild()
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the coordinate origin. Any Points are rotated using this point as reference.
    ''' </summary>
    ''' <value>The origin.</value>
    Public Property Origin As Vector2

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="RotationMatrix" /> class.
    ''' </summary>
    ''' <param name="angle">The angle in radians.</param>
    Public Sub New(ByVal angle As Double)
        Me.New(angle, New Vector2)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="RotationMatrix" /> class.
    ''' </summary>
    ''' <param name="angle">The angle.</param>
    ''' <param name="origin">The coordinate origin.</param>
    Public Sub New(ByVal angle As Double, ByVal origin As Vector2)
        MyBase.New(2, 2)

        _angle = angle
        Me.Origin = origin
        Rebuild()
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="RotationMatrix" /> class.
    ''' </summary>
    Public Sub New()
        Me.New(0)
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Fills the matrix with values.
    ''' </summary>
    Private Sub Rebuild()
        Me(0, 0) = Math.Round(Math.Cos(Angle), 15)
        Me(0, 1) = Math.Round(Math.Sin(Angle), 15)
        Me(1, 0) = Math.Round(-1 * Math.Sin(Angle), 15)
        Me(1, 1) = Math.Round(Math.Cos(Angle), 15)
    End Sub

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="vec">The vec.</param>
    ''' <param name="m">The m.</param>
    ''' <returns>The result of the operator.</returns>
    Public Overloads Shared Operator *(ByVal m As RotationMatrix, ByVal vec As Vector2) As Vector2
        Return New Vector2(m * (vec - m.Origin).ToMatrix + m.Origin)
    End Operator

#End Region
End Class
