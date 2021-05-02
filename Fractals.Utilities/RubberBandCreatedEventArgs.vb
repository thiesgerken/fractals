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

Imports System.Windows

Public Class RubberBandCreatedEventArgs
    Inherits EventArgs

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the rotation angle in radians.
    ''' </summary>
    ''' <value>The rotation angle.</value>
    Public Property RotationAngle As Double

    ''' <summary>
    ''' Gets or sets the region of the created rectangle.
    ''' </summary>
    ''' <value>The region.</value>
    Public Property Region As Rect

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="RubberBandCreatedEventArgs" /> class.
    ''' </summary>
    ''' <param name="region">The region.</param>
    ''' <param name="angle">The angle in radians.</param>
    Public Sub New(ByVal region As Rect, ByVal angle As Double)
        Me.Region = region
        Me.RotationAngle = angle
    End Sub

#End Region
End Class
