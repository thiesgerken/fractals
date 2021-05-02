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
Imports Fluent
Imports Fractals.Mathematics
Imports System.Windows
Imports Fractals.Utilities

''' <summary>
''' Painter coloring pixels with gradients from A to B, where B is applied at n = maxN and A is used at n = 0.
''' </summary>
Public Class GradientPainter
    Inherits Painter

#Region "Private Fields"

    'Byte Array might be faster than a color structure
    Private _highColor As Color = Color.Red
    Private _lowColor As Color = Color.Black

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the color of the high.
    ''' </summary>
    ''' <value>The color of the high.</value>
    Public Property HighColor As System.Windows.Media.Color
        Get
            Return Windows.Media.Color.FromRgb(_highColor.R, _highColor.G, _highColor.B)
        End Get
        Set(ByVal value As System.Windows.Media.Color)
            _highColor = Color.FromArgb(value.R, value.G, value.B)

            OnPropertyChanged("HighColor")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the color of the low.
    ''' </summary>
    ''' <value>The color of the low.</value>
    Public Property LowColor As System.Windows.Media.Color
        Get
            Return Windows.Media.Color.FromRgb(_lowColor.R, _lowColor.G, _lowColor.B)
        End Get
        Set(ByVal value As System.Windows.Media.Color)
            _lowColor = Color.FromArgb(value.R, value.G, value.B)

            OnPropertyChanged("LowColor")
        End Set
    End Property

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
            Dim tab As New RibbonTabItem
            tab.DataContext = Me

            Dim group As New RibbonGroupBox
            group.Header = "Colors"
            group.DataContext = Me

            Dim highColorButton As New DropDownButton
            highColorButton.ToolTip = New ScreenTip With {.Title = "HighColor", .Text = "The color of one end of the gradient."}

            Dim highRect As New System.Windows.Shapes.Rectangle
            highRect.HorizontalAlignment = HorizontalAlignment.Stretch
            highRect.VerticalAlignment = VerticalAlignment.Stretch
            highRect.SetBinding(System.Windows.Shapes.Rectangle.FillProperty, New Binding("HighColor") With {.Converter = New ColorBrushConverter})
            highColorButton.LargeIcon = highRect

            Dim highColorGal As New ColorGallery
            highColorGal.Mode = ColorGalleryMode.StandardColors
            highColorGal.SetBinding(ColorGallery.SelectedColorProperty, New Binding("HighColor") With {.Mode = BindingMode.TwoWay})
            highColorGal.IsNoColorButtonVisible = False
            highColorGal.IsAutomaticColorButtonVisible = False

            highColorButton.Items.Add(highColorGal)
            group.Items.Add(highColorButton)

            Dim lowColorButton As New DropDownButton
            lowColorButton.ToolTip = New ScreenTip With {.Title = "LowColor", .Text = "The color of one end of the gradient."}

            Dim lowRect As New System.Windows.Shapes.Rectangle
            lowRect.HorizontalAlignment = HorizontalAlignment.Stretch
            lowRect.VerticalAlignment = VerticalAlignment.Stretch
            lowRect.SetBinding(System.Windows.Shapes.Rectangle.FillProperty, New Binding("LowColor") With {.Converter = New ColorBrushConverter})
            lowColorButton.LargeIcon = lowRect

            Dim lowColorGal As New ColorGallery
            lowColorGal.Mode = ColorGalleryMode.StandardColors
            lowColorGal.SetBinding(ColorGallery.SelectedColorProperty, New Binding("LowColor") With {.Mode = BindingMode.TwoWay})
            lowColorGal.IsNoColorButtonVisible = False
            lowColorGal.IsAutomaticColorButtonVisible = False

            lowColorButton.Items.Add(lowColorGal)
            group.Items.Add(lowColorButton)

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
            Return "Gradient"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.GradientPainter
        End Get
    End Property

#End Region

#Region "Methods"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("LowColor")
        x.WriteColor(_lowColor)
        x.WriteEndElement()

        x.WriteStartElement("HighColor")
        x.WriteColor(_highColor)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        Dim low As Color = SaveUtils.LoadColor(x.Element("LowColor"))
        Dim high As Color = SaveUtils.LoadColor(x.Element("HighColor"))
    End Sub

#End Region

    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As Cloo.ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))
        For Each arg In positions
            If arg.Key = "color1" Then
                kernel.SetValueArgument(arg.Value, New Vectors.Byte4(_lowColor))
            ElseIf arg.Key = "color2" Then
                kernel.SetValueArgument(arg.Value, New Vectors.Byte4(_highColor))
            End If
        Next
    End Sub

    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Dim dict As New Dictionary(Of String, String)

            dict.Add("color1", "const uchar4")
            dict.Add("color2", "const uchar4")

            Return dict
        End Get
    End Property

    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.GradientPainterSource
        End Get
    End Property
End Class
