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
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging
Imports Cloo
Imports CommandLineParser
Imports Fluent
Imports Fractals.Mathematics
Imports Fractals.Utilities
Imports System.Threading
Imports System.Threading.Tasks
Imports System.Text

Public Class PowerSeries
    Inherits Fractal
    
#Region "Properties"

    ''' <summary>
    ''' Gets the default offset on the viewport.
    ''' </summary>
    ''' <value>The default offset.</value>
    Protected Overrides ReadOnly Property DefaultOffset As ComplexNumber
        Get
            Return 0
        End Get
    End Property

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected Overrides ReadOnly Property DefaultViewportSize As System.Drawing.SizeF
        Get
            Return New SizeF(8, 8)
        End Get
    End Property

    ''' <summary>
    ''' Gets the default escape value.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected Overrides ReadOnly Property DefaultBailoutValue As Double
        Get
            Return 30
        End Get
    End Property

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected Overrides ReadOnly Property DefaultMaxIterationCount As Integer
        Get
            Return 100
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.PowerSeriesKernel
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "PowerSeries"
        End Get
    End Property

    ''' <summary>
    ''' Gets the description.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.Multibrot
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Multibrot" /> class.
    ''' </summary>
    Public Sub New()
        MyBase.New()
    End Sub

#End Region

    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)

    End Sub

    Public Overrides ReadOnly Property RibbonTab As Fluent.RibbonTabItem
        Get
            Return Nothing
        End Get
    End Property

    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)

    End Sub
End Class
