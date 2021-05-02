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

''' <summary>
''' A PaintDataSource that always returns 0.
''' </summary>
Public Class DefaultDataSource
    Inherits DataSource

    ''' <summary>
    ''' Gets a value indicating whether the data source needs the last value of the orbit.
    ''' </summary>
    ''' <value><c>true</c> if the last z is needed; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property NeedsLastZ As Boolean
        Get
            Return False
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this instance is maximum iteration count independent.
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if this instance is maximum iteration count independent; otherwise, <c>false</c>.
    ''' </value>
    Public Overrides ReadOnly Property MaximumIterationCountIndependent As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets the description of this data source.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return "Returns 0."
        End Get
    End Property

    ''' <summary>
    ''' Gets the name of this data source.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "None"
        End Get
    End Property

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.DefaultDataSource
        End Get
    End Property
End Class
