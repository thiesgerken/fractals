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
''' Painter that exclusively paints Newton Fractals according to the root reached.
''' </summary>
Public Class NewtonPainter
    Inherits Painter
    
#Region "Private Fields"

    Private _roots() As ComplexNumber
    Private _colors() As Color
    Private _speedFunction As MathFunction(Of Double, Double)

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the speed function.
    ''' </summary>
    ''' <value>The speed function.</value>
    Public Property SpeedFunction As String
        Get
            Return _speedFunction.StringRepresentation
        End Get
        Set(ByVal value As String)
            _speedFunction = MathFunction(Of Double, Double).Create(value)

            OnPropertyChanged("SpeedFunction")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the roots.
    ''' </summary>
    ''' <value>The roots.</value>
    Public Property Roots As ICollection(Of ComplexNumber)
        Get
            Return _roots
        End Get
        Set(ByVal value As ICollection(Of ComplexNumber))
            _roots = value.ToArray
        End Set
    End Property

    ''' <summary>
    ''' Gets a value indicating whether this painter uses data provided by datasources.
    ''' </summary>
    ''' <value><c>true</c> if this painter needs data; otherwise, <c>false</c>.</value>
    Public Overrides ReadOnly Property NeedsData As Boolean
        Get
            Return False
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
            Return False
        End Get
    End Property

    ''' <summary>
    ''' Gets the name of the Painter.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Root"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.NewtonPainter
        End Get
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="GradientPainter" /> class.
    ''' </summary>
    Public Sub New()
        _roots = New ComplexNumber() {}
        SpeedFunction = "(2.1*x-1)^3*0.5+0.5"
        _colors = New Color() {
                               Color.FromArgb(232, 153, 27),
                               Color.FromArgb(68, 151, 145),
                               Color.FromArgb(183, 29, 17),
                               Color.FromArgb(29, 176, 209),
                               Color.FromArgb(167, 222, 12),
                               Color.FromArgb(2, 123, 177)
                           }
    End Sub

#End Region

#Region "Methods"

    ' ''' <summary>
    ' ''' Gets the color for a given complex number.
    ' ''' </summary>
    ' ''' <param name="result">the iteration result to take the data from.</param>
    ' ''' <returns>the color of the pixel.</returns>
    'Public Overloads Overrides Function GetColor(ByVal result As Utilities.IterationResult) As Color
    '    'lower iteration count -> faster convergence to a root
    '    'use function to change the contrast
    '    Dim speed = _speedFunction(1 - result.IterationCount / ParentInfo.MaximumIterationCount)

    '    If speed > 1 Then speed = 1
    '    If speed < 0 Then speed = 0

    '    'which root is it?
    '    Dim root As Integer = 0
    '    For i As Integer = 0 To _roots.Length - 1
    '        If ComplexNumber.Abs(result.Z - _roots(i)) <= 0.1 Then
    '            root = i
    '            Exit For
    '        End If
    '    Next

    '    Dim color = _colors(root)
    '    Return color.FromArgb(CInt(color.R * speed), CInt(color.G * speed), CInt(color.B * speed))
    'End Function

    ''' <summary>
    ''' Initializes this instance. Called before any pixels are colored.
    ''' </summary>
    Public Overrides Sub Initialize()
        If _colors.Length < _roots.Length Then
            'If the palette is under-determined fill the rest with random colors
            Dim rand As New Random()
            While _colors.Length < _roots.Length
                ReDim Preserve _colors(_colors.Length)

                _colors(_colors.Length - 1) = Color.FromArgb(rand.Next(0, 255), rand.Next(0, 255), rand.Next(0, 255))
            End While
        End If
    End Sub

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("Speed")
        x.WriteValue(SpeedFunction)
        x.WriteEndElement()

        x.WriteStartElement("Colors")

        For Each c In _colors
            x.WriteStartElement("Color")
            x.WriteColor(c)
            x.WriteEndElement()
        Next

        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        SpeedFunction = x.Element("Speed").Value

        Dim cols As New List(Of Color)

        For Each xx In x.Element("Colors").Elements
            cols.Add(SaveUtils.LoadColor(xx))
        Next

        _colors = cols.ToArray
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
