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
''' Structure containing relevant information about a fractal to be passed to a painter 
''' (Of course it would be more convenient to pass the whole fractalbase as 'parent'-property, but this would result in circular references)
''' </summary>
Public Structure FractalInfo
#Region "Properties"

    ''' <summary>
    ''' Gets or sets the size.
    ''' </summary>
    ''' <value>The size.</value>
    Public Property Size As Size

    ''' <summary>
    ''' Gets or sets the maximum iteration count.
    ''' </summary>
    ''' <value>The maximum iteration count.</value>
    Public Property MaximumIterationCount As Integer

    ''' <summary>
    ''' Gets or sets the bailout.
    ''' </summary>
    ''' <value>The bailout.</value>
    Public Property Bailout As Double

    ''' <summary>
    ''' Gets or sets a value indicating whether the iteration counts are smoothed.
    ''' </summary>
    ''' <value><c>true</c> if smoothed; otherwise, <c>false</c>.</value>
    Public Property Smooth As Boolean

    ''' <summary>
    ''' Gets or sets the state of the zoom.
    ''' </summary>
    ''' <value>The state of the zoom.</value>
    Public Property ZoomState As ZoomState

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="FractalInfo" /> struct.
    ''' </summary>
    ''' <param name="size">The size.</param>
    ''' <param name="maximumIterationCount">The maximum iteration count.</param>
    ''' <param name="bailout">The bailout.</param>
    ''' <param name="smooth">if set to <c>true</c> [smooth].</param>
    ''' <param name="zoomstate">The zoomstate.</param>
    Public Sub New(ByVal size As Size, ByVal maximumIterationCount As Integer, ByVal bailout As Double, ByVal smooth As Boolean, ByVal zoomstate As ZoomState)
        Me.Size = size
        Me.MaximumIterationCount = maximumIterationCount
        Me.Bailout = bailout
        Me.Smooth = smooth
        Me.ZoomState = zoomstate
    End Sub

#End Region
End Structure
