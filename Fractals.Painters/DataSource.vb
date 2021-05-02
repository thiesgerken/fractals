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

Imports Cloo
Imports Fractals.Utilities

Public MustInherit Class DataSource

    ''' <summary>
    ''' Gets a value indicating whether the data source needs the last value of the orbit.
    ''' if this is true
    ''' </summary>
    ''' <value><c>true</c> if the last z is needed; otherwise, <c>false</c>.</value>
    Public MustOverride ReadOnly Property NeedsLastZ As Boolean

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public MustOverride ReadOnly Property Source As String
    
    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="clKernel">The kernel.</param>
    ''' <param name="positions">The positions.</param>
    Public Overridable Sub SetArguments(ByVal clKernel As ComputeKernel, ByVal clCommands As ComputeCommandQueue, ByVal positions As Dictionary(Of String, Integer))
    End Sub

    ''' <summary>
    ''' Place to analyze the iteration results (if needed).
    ''' </summary>
    ''' <param name="clCommands">The cl commands.</param>
    ''' <param name="clProgram">The cl program.</param>
    ''' <param name="resultBuffer">The result buffer.</param>
    Public Overridable Sub Init(ByVal clCommands As ComputeCommandQueue, ByVal clProgram As ComputeProgram, ByVal resultBuffer As ComputeBuffer(Of Vectors.Double5), ByVal info As FractalInfo)
    End Sub

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overridable ReadOnly Property Arguments As Dictionary(Of String, String)
        Get
            Return New Dictionary(Of String, String)
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this instance is maximum iteration count independent.
    ''' This affects the automatic adjustment of the maximum iteration count, a higher count is applied when this returns 'true'
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if this instance is maximum iteration count independent; otherwise, <c>false</c>.
    ''' </value>
    Public MustOverride ReadOnly Property MaximumIterationCountIndependent As Boolean

    ''' <summary>
    ''' Gets the name of this data source.
    ''' </summary>
    ''' <value>The name.</value>
    Public MustOverride ReadOnly Property Name As String

    ''' <summary>
    ''' Gets the description of this data source.
    ''' </summary>
    ''' <value>The description.</value>
    Public MustOverride ReadOnly Property Description As String

End Class
