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
Imports System.Resources
Imports System.Reflection
Imports System.Drawing.Imaging
Imports System.Runtime.InteropServices
Imports Fractals.Utilities
Imports System.Globalization
Imports System.Xml
Imports Microsoft.VisualBasic

''' <summary>
''' Represents a color palette used by PalettePainter
''' </summary>
Public Class ColorPalette
#Region "Fields"

    Private _id As String = ""
    Private _colorsUsed As List(Of KeyValuePair(Of Integer, Color))
    Private _colors() As Byte  'Byte Array similar to bitmap data (32bppRGB) order: (b, g, r, 0)

    Private Shared _rm As New ResourceManager("Fractals.Utilities.Palettes", Assembly.GetAssembly(GetType(ColorPalette)))

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets the count of colors in the palette.
    ''' </summary>
    ''' <value>The amount of colors.</value>
    Public ReadOnly Property Length As Integer
        Get
            Return CInt(_colors.Length / 4)
        End Get
    End Property

    ''' <summary>
    ''' Gets the color in the palette at a specified index.
    ''' </summary>
    ''' <param name="index">The index of the desired color.</param>
    ''' <returns></returns>
    Default Public ReadOnly Property Item(ByVal index As Integer) As Color
        Get
            Return GetColor(index)
        End Get
    End Property

    ''' <summary>
    ''' Gets the color in the palette at a specified position.
    ''' </summary>
    ''' <param name="indexPercentage">The index percentage (1 = last color, 0 = first color, 0.32442 = another color).</param>
    ''' <returns></returns>
    Default Public ReadOnly Property Item(ByVal indexPercentage As Double) As Color
        Get
            Return GetColor(indexPercentage)
        End Get
    End Property

    ''' <summary>
    ''' Gets the available palettes.
    ''' </summary>
    ''' <value>The available palettes.</value>
    Public Shared ReadOnly Property AvailablePalettes As List(Of String)
        Get
            Dim l As New List(Of String)

            _rm.GetObject("red")
            
            Dim id = _rm.GetResourceSet(CultureInfo.CurrentUICulture, False, True).GetEnumerator

            'Iterate through the ResourceSet and display the contents to the console. 
            While id.MoveNext()
                l.Add(id.Key.ToString)
            End While

            l.Sort()

            Return l
        End Get
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ColorPalette" /> class.
    ''' </summary>
    Public Sub New()
        ReDim _colors(3)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ColorPalette" /> class.
    ''' </summary>
    ''' <param name="colors">The colors.</param>
    Public Sub New(ByVal colors() As Byte)
        _colors = colors
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ColorPalette" /> class from an embedded resource.
    ''' </summary>
    ''' <param name="paletteId">The palette id.</param>
    Public Sub New(ByVal paletteId As String)
        LoadEmbedded(paletteId)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ColorPalette" /> class from a given bitmap.
    ''' </summary>
    ''' <param name="palette">The palette.</param>
    ''' <remarks>If the image's height is more than 1 pixel, only the first row is used.</remarks>
    Public Sub New(ByVal palette As Bitmap)
        LoadImage(palette)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ColorPalette" /> class.
    ''' </summary>
    ''' <param name="colors">The colors to use with their position in the palette.</param>
    ''' <param name="count">the length of the palette.</param>
    ''' <remarks>the length of the palette is determined by the highest position provided.</remarks>
    Public Sub New(ByVal colors As List(Of KeyValuePair(Of Integer, Color)), ByVal count As Integer)
        LoadList(colors, count)
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Loads the list.
    ''' </summary>
    ''' <param name="colors">The colors.</param>
    ''' <param name="count">The count.</param>
    Private Sub LoadList(ByVal colors As List(Of KeyValuePair(Of Integer, Color)), ByVal count As Integer)
        colors.Sort(Function(first, second) first.Key.CompareTo(second.Key))

        _colorsUsed = colors

        Dim lowestIndex = colors(0).Key
        Dim highestIndex = colors(colors.Count - 1).Key
        ReDim _colors(count * 4 - 1)

        Dim mix = Function(a As Integer, b As Integer, percentage As Double) CByte(a + percentage * (b - a))

        For i As Integer = 0 To count - 1
            Dim index = i / (count - 1) * highestIndex

            'find last color (pos <= i) and next color (pos >= i)
            Dim lastColor As KeyValuePair(Of Integer, Color)
            Dim nextColor As New KeyValuePair(Of Integer, Color)(-1, Color.Black)

            For Each kvp In colors
                If kvp.Key <= index + lowestIndex Then
                    lastColor = kvp
                Else
                    nextColor = kvp
                    Exit For
                End If
            Next

            If nextColor.Key = -1 Then nextColor = lastColor 'not every pixel has a next color (last pixel!)

            Dim percentage = (index - lastColor.Key + lowestIndex) / (nextColor.Key - lastColor.Key)
            If nextColor.Key - lastColor.Key = 0 Then percentage = 1

            _colors(4 * i) = mix(lastColor.Value.B, nextColor.Value.B, percentage)
            _colors(4 * i + 1) = mix(lastColor.Value.G, nextColor.Value.G, percentage)
            _colors(4 * i + 2) = mix(lastColor.Value.R, nextColor.Value.R, percentage)
        Next
    End Sub

    ''' <summary>
    ''' Loads an image into the color array.
    ''' </summary>
    ''' <param name="image">The image to load.</param>
    Private Sub LoadImage(ByVal image As Bitmap)
        ReDim _colors(image.Width * 4 - 1) 'only load first row since we do not need more pixels

        Dim bitmapData = image.LockBits(New Rectangle(0, 0, image.Width, 1), ImageLockMode.ReadOnly, PixelFormat.Format32bppRgb)
        Marshal.Copy(bitmapData.Scan0, _colors, 0, image.Width * 4)
        image.UnlockBits(bitmapData)
    End Sub

    ''' <summary>
    ''' Loads an embedded image.
    ''' </summary>
    ''' <param name="id">The id to load.</param>
    Private Sub LoadEmbedded(ByVal id As String)
        _id = id.ToLowerInvariant

        Dim obj = _rm.GetObject(id)
        If obj Is Nothing Then Throw New ArgumentException("The requested Palette with id '" & id & "' could not be found")

        LoadImage(CType(obj, Bitmap))
    End Sub

    ''' <summary>
    ''' Gets the color in the palette at a specified index.
    ''' </summary>
    ''' <param name="index">The zero-based index of the desired color.</param>
    ''' <returns></returns>
    Public Function GetColor(ByVal index As Integer) As Color
        If index < 0 Or index >= Length Then Throw New ArgumentException("The Index has to be in the interval [0;Length[")

        index *= 4 'Translate to bitmap bytes

        Return Color.FromArgb(_colors(index + 2), _colors(index + 1), _colors(index))
    End Function

    ''' <summary>
    ''' Gets the color in the palette at a specified position.
    ''' </summary>
    ''' <param name="indexPercentage">The index percentage (1 = last color, 0 = first color, 0.32442 = another color).</param>
    ''' <returns></returns>
    Public Function GetColor(ByVal indexPercentage As Double) As Color
        If indexPercentage < 0 Or indexPercentage > 1 Then Throw New ArgumentException("The Index percentage has to be in the interval [0;1]")

        Dim index = CInt(indexPercentage * (Length - 1)) * 4

        Return Color.FromArgb(_colors(index + 2), _colors(index + 1), _colors(index))
    End Function

    ''' <summary>
    ''' Converts this color palette to a bitmap.
    ''' </summary>
    ''' <returns>a bitmap with a height of 1 pixel containing the palette.</returns>
    Public Function ToBitmap() As Bitmap
        Return ToBitmap(1)
    End Function

    ''' <summary>
    ''' Converts this color palette to a bitmap with a specified height.
    ''' </summary>
    ''' <param name="height">The height, in pixels.</param>
    ''' <returns></returns>
    Public Function ToBitmap(ByVal height As Integer) As Bitmap
        Dim result As New Bitmap(Length, height, PixelFormat.Format32bppRgb)

        Dim bitmapBytes(height * _colors.Length - 1) As Byte

        For i As Integer = 0 To height - 1
            _colors.CopyTo(bitmapBytes, i * _colors.Length)
        Next

        Dim bitmapData = result.LockBits(New Rectangle(0, 0, Length, height), ImageLockMode.WriteOnly, PixelFormat.Format32bppRgb)
        Marshal.Copy(bitmapBytes, 0, bitmapData.Scan0, height * _colors.Length)
        result.UnlockBits(bitmapData)

        Return result
    End Function

    ''' <summary>
    ''' Converts this color palette to a bitmap with a specified size.
    ''' </summary>
    ''' <param name="size">The desired size.</param>
    ''' <returns></returns>
    Public Function ToBitmap(ByVal size As Size) As Bitmap
        Dim result As New Bitmap(size.Width, size.Height, PixelFormat.Format32bppRgb)

        Dim unscaled = ToBitmap(size.Height)

        Using g = Graphics.FromImage(result)
            g.DrawImage(unscaled, New Rectangle(New Point, size))
        End Using

        Return result
    End Function

    ''' <summary>
    ''' Parses the specified value.
    ''' </summary>
    ''' <param name="value">The value.</param>
    ''' <param name="format">The format.</param>
    ''' <returns></returns>
    Public Shared Function Parse(ByVal value As String, ByVal format As CultureInfo) As ColorPalette
        If value.StartsWith("id:") Then
            Return New ColorPalette(value.Substring(3))
        ElseIf value.StartsWith("(") And value.EndsWith(")") Then
            Dim colors As New List(Of KeyValuePair(Of Integer, Color))

            For Each kvp In value.Substring(1, value.Length - 2).Split(";"c)
                Dim location As Integer = Integer.Parse(kvp.Split("="c)(0))
                Dim colString = kvp.Split("="c)(1)
                Dim col As Color

                If colString.StartsWith("#"c) Then
                    If Not colString.Length = 7 Then Throw New Exception("'" & colString & "' is not recognized as a color.")

                    Dim r = Byte.Parse(colString.Substring(1, 2), NumberStyles.HexNumber)
                    Dim g = Byte.Parse(colString.Substring(3, 2), NumberStyles.HexNumber)
                    Dim b = Byte.Parse(colString.Substring(5, 2), NumberStyles.HexNumber)

                    col = Color.FromArgb(r, g, b)
                Else
                    col = Color.FromName(colString)
                End If

                colors.Add(New KeyValuePair(Of Integer, Color)(location, col))
            Next

            Return New ColorPalette(colors, 256)
        Else
            Return New ColorPalette(New Bitmap(value))
        End If
    End Function
    
    ''' <summary>
    ''' Implements the operator =.
    ''' </summary>
    ''' <param name="pal1">The pal1.</param>
    ''' <param name="pal2">The pal2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator =(ByVal pal1 As ColorPalette, ByVal pal2 As ColorPalette) As Boolean
        If pal1._colors.Length <> pal2._colors.Length Then Return False

        For i As Integer = 0 To pal1._colors.Length - 1
            If pal1._colors(i) <> pal2._colors(i) Then Return False
        Next

        Return True
    End Operator

    ''' <summary>
    ''' Implements the operator &lt;&gt;.
    ''' </summary>
    ''' <param name="pal1">The pal1.</param>
    ''' <param name="pal2">The pal2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator <>(ByVal pal1 As ColorPalette, ByVal pal2 As ColorPalette) As Boolean
        Return Not pal1 = pal2
    End Operator

#End Region

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As ColorPalette
        Dim type = x.Element("Type").Value

        Select Case type
            Case "PartialPalette"
                Dim cols As New List(Of KeyValuePair(Of Integer, Color))

                For Each xx In x.Element("Data").Elements("Item")
                    cols.Add(New KeyValuePair(Of Integer, Color)(CInt(xx.Element("Position").Value), SaveUtils.LoadColor(xx.Element("Color"))))
                Next

                Return New ColorPalette(cols, CInt(x.Element("Length").Value))
            Case "CompletePalette"
                Dim cols As New List(Of Byte)

                For Each xx In x.Element("Data").Elements("B")
                    cols.Add(CByte(xx.Value))
                Next

                Return New ColorPalette(cols.ToArray)
            Case "Embedded"
                Return New ColorPalette(x.Element("ID").ToString)
            Case Else
                Return New ColorPalette("colorful1")
        End Select
    End Function

    ''' <summary>
    ''' Saves the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Public Sub Save(ByVal x As XmlWriter)
        If Not _colorsUsed Is Nothing Then
            'save colors used

            x.WriteStartElement("Type")
            x.WriteValue("PartialPalette")
            x.WriteEndElement()

            x.WriteStartElement("Length")
            x.WriteValue(_colors.Length)
            x.WriteEndElement()

            x.WriteStartElement("Data")

            For Each kvp In _colorsUsed
                x.WriteStartElement("Item")

                x.WriteStartElement("Position")
                x.WriteValue(kvp.Key)
                x.WriteEndElement()

                x.WriteStartElement("Color")
                x.WriteColor(kvp.Value)
                x.WriteEndElement()

                x.WriteEndElement()
            Next

            x.WriteEndElement()
        ElseIf _id = "" Then
            'save palette

            x.WriteStartElement("Type")
            x.WriteValue("CompletePalette")
            x.WriteEndElement()

            x.WriteStartElement("Data")

            For i As Integer = 0 To _colors.Length - 1
                x.WriteStartElement("B")
                x.WriteValue(_colors(i))
                x.WriteEndElement()
            Next

            x.WriteEndElement()
        ElseIf _id <> "" Then
            'just save id since the palette is embedded

            x.WriteStartElement("Type")
            x.WriteValue("Embedded")
            x.WriteEndElement()

            x.WriteStartElement("ID")
            x.WriteValue(_id)
            x.WriteEndElement()
        End If
    End Sub

    ''' <summary>
    ''' Returns an instance with data from a binary reader.
    ''' </summary>
    ''' <param name="reader">The binary reader.</param>
    Public Sub Load(ByVal reader As System.IO.BinaryReader)
        Dim type = reader.ReadInt32

        If type = 0 Then
            'raw
            ReDim _colors(reader.ReadInt32 - 1)

            For i As Integer = 0 To _colors.Length - 1
                _colors(i) = reader.ReadByte
            Next
        ElseIf type = 1 Then
            'color list
            Dim list As New List(Of KeyValuePair(Of Integer, Color))

            Dim length = reader.ReadInt32

            For i As Integer = 0 To reader.ReadInt32 - 1
                Dim pos = reader.ReadInt32

                Dim r = reader.ReadByte
                Dim g = reader.ReadByte
                Dim b = reader.ReadByte

                list.Add(New KeyValuePair(Of Integer, Color)(pos, Color.FromArgb(r, g, b)))
            Next

            LoadList(list, length)
        Else
            'embedded
            LoadEmbedded(reader.ReadString())
        End If
    End Sub

    ''' <summary>
    ''' Saves this instance to a binary writer.
    ''' </summary>
    ''' <param name="writer">The writer to write on.</param>
    Public Sub Save(ByVal writer As System.IO.BinaryWriter)
        If Not _colorsUsed Is Nothing Then
            'save colors used
            writer.Write(1)
            writer.Write(_colors.Length)
            writer.Write(_colorsUsed.Count)

            For Each kvp In _colorsUsed
                writer.Write(kvp.Key)

                writer.Write(kvp.Value.R)
                writer.Write(kvp.Value.G)
                writer.Write(kvp.Value.B)
            Next
        ElseIf _id = "" Then
            'save palette
            writer.Write(0) 'raw
            writer.Write(_colors.Length)

            For i As Integer = 0 To _colors.Length - 1
                writer.Write(_colors(i))
            Next
        ElseIf _id <> "" Then
            'just save id since the palette is embedded
            writer.Write(2) 'embedded
            writer.Write(_id)
        End If
    End Sub
End Class
