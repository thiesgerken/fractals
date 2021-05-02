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
Imports Fractals.Utilities

''' <summary>
''' Painter that shows lines where the iteration count is near an integral value.
''' </summary>
Public Class EquiPotentialPainter
    Inherits Painter
    
#Region "Private Fields"

    Private _lineSpacing As Double
    Private _threshold As Double

    'parameters
    Private _fillColor As Color
    Private _lineColor As Color
    Private _lineCount As Integer = 20
    Private _lineWidth As Double = 0.03
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
            Return "EqualPotential"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return New Bitmap(1, 1)
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="EquiPotentialPainter" /> class.
    ''' </summary>
    Public Sub New()
        _fillColor = Color.Black
        _lineColor = Color.Blue
        _lineCount = 10
        _lineWidth = 0.1
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Initializes this instance. Called before any pixels are colored.
    ''' </summary>
    Public Overrides Sub Initialize()
        Dim max = 1

        _lineSpacing = max / _lineCount
        _threshold = _lineSpacing * _lineWidth
    End Sub

    ' ''' <summary>
    ' ''' Gets the color for a given complex number.
    ' ''' </summary>
    ' ''' <param name="result">the iteration result to take the data from.</param>
    ' ''' <returns>the color of the pixel.</returns>
    'Public Overloads Overrides Function GetColor(ByVal result As Utilities.IterationResult) As Color
    '    Dim data As Double = 1 ' Me.DataSource.GetData(ParentInfo, result)

    '    If data Mod _lineSpacing <= _threshold Or data Mod _lineSpacing >= _lineSpacing - _threshold Then
    '        Return _lineColor
    '    End If

    '    Return _fillColor
    'End Function

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("FillColor")
        x.WriteColor(_fillColor)
        x.WriteEndElement()

        x.WriteStartElement("LineColor")
        x.WriteColor(_lineColor)
        x.WriteEndElement()

        x.WriteStartElement("LineWidth")
        x.WriteValue(_lineWidth)
        x.WriteEndElement()

        x.WriteStartElement("LineCount")
        x.WriteValue(_lineCount)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        _lineWidth = Double.Parse(x.Element("LineWidth").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _lineCount = Integer.Parse(x.Element("LineCount").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _lineColor = SaveUtils.LoadColor(x.Element("LineColor"))
        _fillColor = SaveUtils.LoadColor(x.Element("FillColor"))
    End Sub

#End Region

    Public Overrides ReadOnly Property Source As String
        Get
            Return ""
        End Get
    End Property

    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Return New Dictionary(Of String, String)
        End Get
    End Property

    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As Cloo.ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))

    End Sub
End Class
