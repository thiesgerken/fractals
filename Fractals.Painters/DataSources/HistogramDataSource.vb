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
Imports System.Runtime.InteropServices

''' <summary>
''' Uses a histogram
''' </summary>
Public Class HistogramDataSource
    Inherits DataSource

    Private Const _resolution As Integer = 100

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
            Return "Builds a histogram of the fractal's iteration counts and uses the integral function of this histogramm as datasource."
        End Get
    End Property

    ''' <summary>
    ''' Gets the name of this data source.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "IterationCount (Histogram)"
        End Get
    End Property

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property Arguments As Dictionary(Of String, String)
        Get
            Dim dict As New Dictionary(Of String, String)

            dict.Add("cdf", "global int*")
            dict.Add("count", "const int")
            dict.Add("res", "const int")

            Return dict
        End Get
    End Property

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="clKernel">The kernel.</param>
    ''' <param name="clCommands"></param>
    ''' <param name="positions">The positions.</param>
    Public Overrides Sub SetArguments(ByVal clKernel As ComputeKernel, ByVal clCommands As ComputeCommandQueue, ByVal positions As Dictionary(Of String, Integer))
        For Each arg In positions
            If arg.Key = "cdf" Then
                If _cdfBuffer IsNot Nothing Then _cdfBuffer.Dispose()

                _cdfGC = GCHandle.Alloc(_cdf, GCHandleType.Pinned)
                _cdfBuffer = New ComputeBuffer(Of Integer)(clKernel.Context, ComputeMemoryFlags.ReadOnly, _cdf.Length)

                clCommands.Write(_cdfBuffer, True, 0, _cdf.Length, _cdfGC.AddrOfPinnedObject, Nothing)
                clCommands.Finish()

                clKernel.SetMemoryArgument(arg.Value, _cdfBuffer)

                ReDim _cdf(0)
                _cdfGC.Free()
            ElseIf arg.Key = "count" Then
                clKernel.SetValueArgument(arg.Value, _count)
            ElseIf arg.Key = "res" Then
                clKernel.SetValueArgument(arg.Value, _resolution)
            End If
        Next
    End Sub

    Private _count As Integer
    Private _cdf() As Integer
    Private _cdfGC As GCHandle
    Private _cdfBuffer As ComputeBuffer(Of Integer)
   
    Public Overrides Sub Init(ByVal clCommands As ComputeCommandQueue, ByVal clProgram As ComputeProgram, ByVal resultBuffer As ComputeBuffer(Of Vectors.Double5), ByVal info As FractalInfo)
        Dim histogramKernel = clProgram.CreateKernel("CreateHistogram")
        Dim histogramBuffer As New ComputeBuffer(Of Integer)(clProgram.Context, ComputeMemoryFlags.ReadWrite, _resolution * info.MaximumIterationCount)
        Dim histogram(_resolution * info.MaximumIterationCount - 1) As Integer
        Dim histGC = GCHandle.Alloc(histogram, GCHandleType.Pinned)

        histogramKernel.SetValueArgument(0, _resolution)
        histogramKernel.SetMemoryArgument(1, resultBuffer)
        histogramKernel.SetMemoryArgument(2, histogramBuffer)

        clCommands.Execute(histogramKernel, {0}, {resultBuffer.Count}, Nothing, New ComputeEventList)
        clCommands.Read(histogramBuffer, True, 0, histogram.Length, histGC.AddrOfPinnedObject, New ComputeEventList)
        clCommands.Finish()

        _count = 0
        ReDim _cdf(_resolution * info.MaximumIterationCount - 1)

        For i As Integer = 0 To histogram.Length - 1
            _count += histogram(i)

            _cdf(i) = _count
        Next

        ReDim histogram(0)
        histGC.Free()
        histogramBuffer.Dispose()
        histogramKernel.Dispose()
    End Sub

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.HistogramDataSourceSource
        End Get
    End Property
End Class
