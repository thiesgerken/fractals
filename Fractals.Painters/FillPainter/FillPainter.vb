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
Imports System.Windows.Data
Imports Cloo
Imports Fluent
Imports System.Windows
Imports Fractals.Utilities

''' <summary>
''' Painter that colors every pixel with the same color
''' </summary>
Public Class FillPainter
    Inherits Painter
    
#Region "Fields"
    Private _fillColor As Color = Color.Black

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the color of the fill.
    ''' </summary>
    ''' <value>The color of the fill.</value>
    Public Property FillColor As System.Windows.Media.Color
        Get
            Return Windows.Media.Color.FromRgb(_fillColor.R, _fillColor.G, _fillColor.B)
        End Get
        Set(ByVal value As System.Windows.Media.Color)
            _fillColor = Color.FromArgb(value.R, value.G, value.B)

            OnPropertyChanged("FillColor")
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
            Dim tab As New RibbonTabItem
            tab.DataContext = Me

            Dim group As New RibbonGroupBox
            group.Header = "Color"
            group.DataContext = Me

            Dim colorButton As New DropDownButton
            colorButton.ToolTip = New ScreenTip With {.Title = "FillColor", .Text = "The color which is used to paint the fractal."}

            Dim rect As New System.Windows.Shapes.Rectangle
            rect.HorizontalAlignment = HorizontalAlignment.Stretch
            rect.VerticalAlignment = VerticalAlignment.Stretch
            rect.SetBinding(System.Windows.Shapes.Rectangle.FillProperty, New Binding("FillColor") With {.Converter = New ColorBrushConverter})
            colorButton.LargeIcon = rect

            Dim colorGal As New ColorGallery
            colorGal.Mode = ColorGalleryMode.StandardColors
            colorGal.SetBinding(ColorGallery.SelectedColorProperty, New Binding("FillColor") With {.Mode = BindingMode.TwoWay})
            colorGal.IsNoColorButtonVisible = False
            colorGal.IsAutomaticColorButtonVisible = False

            colorButton.Items.Add(colorGal)
            group.Items.Add(colorButton)
            tab.Groups.Add(group)
            Return tab
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
            Return "Fill"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.FillPainter
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="GradientPainter" /> class.
    ''' </summary>
    Public Sub New()
        _fillColor = Color.Black
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("Color")
        x.WriteColor(_fillColor)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        _fillColor = SaveUtils.LoadColor(x.Element("Color"))
    End Sub

#End Region

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Dim dict As New Dictionary(Of String, String)

            dict.Add("color", "const uchar4")

            Return dict
        End Get
    End Property

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    ''' <param name="commands"></param>
    ''' <param name="positions">The positions.</param>
    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))
        For Each arg In positions
            If arg.Key = "color" Then
                kernel.SetValueArgument(arg.Value, New Vectors.Byte4(_fillColor))
            End If
        Next
    End Sub

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.FillPainterSource
        End Get
    End Property
End Class
