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
Imports CommandLineParser
Imports Fluent
Imports System.Windows
Imports Fractals.Utilities
Imports System.Runtime.InteropServices
Imports Cloo

''' <summary>
''' Painter that colors the pixels like the light spectrum.
''' </summary>
Public Class PalettePainter
    Inherits Painter

#Region "Private Fields"

    Private _palette As ColorPalette
    Private _phasePercentage As Double
    Private _cycleCount As Double

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the palette.
    ''' </summary>
    ''' <value>The palette.</value>
    Public Property Palette As ColorPalette
        Get
            Return _palette
        End Get
        Set(ByVal value As ColorPalette)
            _palette = value

            OnPropertyChanged("Palette")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the cycle count.
    ''' </summary>
    ''' <value>The cycle count.</value>
    Public Property CycleCount As Double
        Get
            Return _cycleCount
        End Get
        Set(ByVal value As Double)
            _cycleCount = value

            OnPropertyChanged("CycleCount")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the phase.
    ''' </summary>
    ''' <value>The phase.</value>
    Public Property Phase As Double
        Get
            Return _phasePercentage * 100
        End Get
        Set(ByVal value As Double)
            _phasePercentage = value / 100

            OnPropertyChanged("Phase")
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

            Dim groupPalette As New RibbonGroupBox
            groupPalette.Header = "Palette"
            groupPalette.DataContext = Me

            Dim paletteButton As New DropDownButton
            paletteButton.ToolTip = New ScreenTip With {.Title = "Palette", .Text = "The palette that is used to paint the fractal."}

            Dim gal As New Gallery
            gal.MaxItemsInRow = 1
            gal.ItemHeight = 15
            gal.ItemWidth = 230
            gal.GroupBy = "Tag"

            For Each id In ColorPalette.AvailablePalettes
                Dim item As New GalleryItem

                Dim img As New Windows.Controls.Image
                img.Source = New ColorPalette(id).ToBitmap(New Drawing.Size(240, 15)).ToBitmapSource
                img.Tag = New ColorPalette(id)

                item.Content = img
                item.Tag = "Built-in"
                item.ToolTip = New ScreenTip With {.Text = id, .Title = "Built-in Palette"}

                gal.Items.Add(item)

                If Palette = CType(img.Tag, ColorPalette) Then
                    gal.SelectedItem = item
                    paletteButton.LargeIcon = CType(img.Tag, ColorPalette).ToBitmap(New Drawing.Size(32, 32)).ToBitmapSource
                End If

                AddHandler item.Selected, Sub(s, e)
                                              paletteButton.LargeIcon = CType(img.Tag, ColorPalette).ToBitmap(New Drawing.Size(32, 32)).ToBitmapSource
                                              Me.Palette = CType(img.Tag, ColorPalette)
                                          End Sub
            Next

            paletteButton.Items.Add(gal)
            groupPalette.Items.Add(paletteButton)
            tab.Groups.Add(groupPalette)

            Dim groupMapping As New RibbonGroupBox
            groupMapping.Header = "Mapping"

            Dim textBoxCycles As New TextBox
            textBoxCycles.SetBinding(TextBox.TextProperty, "CycleCount")
            textBoxCycles.InputWidth = 75
            textBoxCycles.Header = "Cycles: "
            textBoxCycles.ToolTip = New ScreenTip With {.Title = "Cycle Count", .Text = "Count of how often the palette is mapped from data = 0 to data = 1."}
            groupMapping.Items.Add(textBoxCycles)

            Dim textBoxPhase As New TextBox
            textBoxPhase.SetBinding(TextBox.TextProperty, "Phase")
            textBoxPhase.InputWidth = 75
            textBoxPhase.Header = "Phase: "
            textBoxPhase.ToolTip = New ScreenTip With {.Title = "Phase (%)", .Text = "This parameter allows shifting of the palette colors."}
            groupMapping.Items.Add(textBoxPhase)

            tab.Groups.Add(groupMapping)

            Return tab
        End Get
    End Property

    ''' <summary>
    ''' Gets the arguments (name, type).
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property Arguments As System.Collections.Generic.Dictionary(Of String, String)
        Get
            Dim dict As New Dictionary(Of String, String)

            dict.Add("palette", "global uchar4*")
            dict.Add("length", "const int")
            dict.Add("cycles", "const double")
            dict.Add("phase", "const double")

            Return dict
        End Get
    End Property

    Private _paletteArgument As New ValueArgument(Of Utilities.ColorPalette)("palette", "", Nothing, "Colorpalette ('id:[name]' (see below for ids), the location of a bitmap file, or '([location1]=[color1];[location2]=[color2];...)')")
    Private _phaseArgument As New ValueArgument(Of Double)("phase", "", 0, "Phase Percentage")
    Private _cyclesArgument As New ValueArgument(Of Double)("cycles", "", 1, "Cycle count")

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property CommandLineArguments As Argument()
        Get
            Return New Argument() {_paletteArgument, _phaseArgument, _cyclesArgument}
        End Get
    End Property

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overrides Sub EvaluateCommandLineArguments()
        If _paletteArgument.IsParsed Then
            Palette = _paletteArgument.Value
        End If

        If _phaseArgument.IsParsed Then
            _phasePercentage = _phaseArgument.Value
        End If

        If _cyclesArgument.IsParsed Then
            _cycleCount = _cyclesArgument.Value
        End If
    End Sub

    Dim pal() As Vectors.Byte4
    Dim palGC As GCHandle
    Dim palBuffer As ComputeBuffer(Of Vectors.Byte4)

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    ''' <param name="commands"></param>
    ''' <param name="positions">The positions.</param>
    Public Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel, ByVal commands As ComputeCommandQueue, ByVal positions As System.Collections.Generic.Dictionary(Of String, Integer))
        For Each arg In positions
            If arg.Key = "palette" Then
                ReDim pal(Palette.Length - 1)

                For i As Integer = 0 To Palette.Length - 1
                    pal(i) = New Vectors.Byte4(_palette(i))
                Next

                If palBuffer IsNot Nothing Then palBuffer.Dispose()

                palGC = GCHandle.Alloc(pal, GCHandleType.Pinned)
                palBuffer = New ComputeBuffer(Of Vectors.Byte4)(kernel.Context, ComputeMemoryFlags.ReadOnly, pal.Length)

                commands.Write(palBuffer, True, 0, pal.Length, palGC.AddrOfPinnedObject, Nothing)
                commands.Finish()

                kernel.SetMemoryArgument(arg.Value, palBuffer)

                ReDim pal(0)
                palGC.Free()
            ElseIf arg.Key = "length" Then
                kernel.SetValueArgument(arg.Value, Me.Palette.Length)
            ElseIf arg.Key = "cycles" Then
                kernel.SetValueArgument(arg.Value, _cycleCount)
            ElseIf arg.Key = "phase" Then
                kernel.SetValueArgument(arg.Value, _phasePercentage)
            End If
        Next
    End Sub

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
            Return "Palette"
        End Get
    End Property

    ''' <summary>
    ''' Gets a preview image to display in the GUI.
    ''' </summary>
    ''' <value>The preview image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.PalettePainter
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="PalettePainter" /> class.
    ''' </summary>
    Public Sub New()
        _phasePercentage = 0.2
        _palette = New ColorPalette(ColorPalette.AvailablePalettes(0))
        _cycleCount = 2.3
    End Sub

#End Region

#Region "Overridden Methods"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("Palette")
        _palette.Save(x)
        x.WriteEndElement()

        x.WriteStartElement("Phase")
        x.WriteValue(_phasePercentage)
        x.WriteEndElement()

        x.WriteStartElement("Cycles")
        x.WriteValue(_cycleCount)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        _phasePercentage = Double.Parse(x.Element("Phase").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _cycleCount = Double.Parse(x.Element("Cycles").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        _palette = ColorPalette.Load(x.Element("Palette"))
    End Sub

#End Region

    ''' <summary>
    ''' Gets the openCL source code for this datasource.
    ''' </summary>
    ''' <value>The source.</value>
    Public Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.PalettePainterSource
        End Get
    End Property
End Class
