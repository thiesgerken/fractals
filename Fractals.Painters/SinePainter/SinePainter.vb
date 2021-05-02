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
Imports Fluent
Imports Fractals.Mathematics
Imports Fractals.Utilities

''' <summary>
''' Painter that uses the sine of the iteration count to color pixels. 
''' (sin(scale*n + phase))
''' </summary>
Public Class SinePainter
    Inherits Painter
    
#Region "Private Fields"

    Private _scaleR As Double
    Private _scaleG As Double
    Private _scaleB As Double

    Private _phaseR As Double
    Private _phaseG As Double
    Private _phaseB As Double


#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="SinePainter" /> class.
    ''' </summary>
    Public Sub New()
        _scaleR = 2
        _scaleG = 4
        _scaleB = 6
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("ScaleR")
        x.WriteValue(_scaleR)
        x.WriteEndElement()

        x.WriteStartElement("ScaleG")
        x.WriteValue(_scaleG)
        x.WriteEndElement()

        x.WriteStartElement("ScaleB")
        x.WriteValue(_scaleB)
        x.WriteEndElement()

        x.WriteStartElement("PhaseR")
        x.WriteValue(_phaseR)
        x.WriteEndElement()

        x.WriteStartElement("PhaseG")
        x.WriteValue(_phaseG)
        x.WriteEndElement()

        x.WriteStartElement("PhaseB")
        x.WriteValue(_phaseB)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        _scaleR = Double.Parse(x.Element("ScaleR").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _scaleG = Double.Parse(x.Element("ScaleG").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _scaleB = Double.Parse(x.Element("ScaleB").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)

        _phaseR = Double.Parse(x.Element("PhaseR").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _phaseG = Double.Parse(x.Element("PhaseG").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _phaseB = Double.Parse(x.Element("PhaseB").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
    End Sub

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets a value indicating whether this painter uses data provided by datasources.
    ''' </summary>
    ''' <value><c>true</c> if this painter needs data; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property NeedsData As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this painter is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As RibbonTabItem
        Get
            Return Nothing
        End Get
    End Property

    ''' <summary>
    ''' Gets the description of the Painter.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this <see cref="Painter" /> is selectable in the GUI.
    ''' If this retruns <c>false</c>, this painter will only be shown if the current calculator specified it as an additional painter.
    ''' </summary>
    ''' <value><c>true</c> if selectable; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property Selectable As Boolean
        Get
            Return True
        End Get
    End Property

    ''' <summary>
    ''' Gets the name of the Painter.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Sine"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.SinePainter
        End Get
    End Property

#End Region

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.SinePainterSource
        End Get
    End Property

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Dim dict As New Dictionary(Of String, String)

            dict.Add("scale", "const double4")
            dict.Add("phase", "const double4")
          
            Return dict
        End Get
    End Property

    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As Cloo.ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))
        For Each arg In positions
            If arg.Key = "scale" Then
                kernel.SetValueArgument(arg.Value, New Vectors.Double4(_scaleR, _scaleG, _scaleB, 0))
            ElseIf arg.Key = "phase" Then
                kernel.SetValueArgument(arg.Value, New Vectors.Double4(_phaseR, _phaseG, _phaseB, 0))
            End If
        Next
    End Sub
End Class
