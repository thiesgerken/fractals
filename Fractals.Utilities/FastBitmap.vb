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

Imports System.IO
Imports System.Text
Imports System.Drawing

Public Class FastBitmap

    ''' <summary>
    ''' Gets the size of the file.
    ''' </summary>
    ''' <value>The size of the file.</value>
    Private ReadOnly Property FileSize As UInt32
        Get
            'Header Size + Data Size + Palette Size
            Return CUInt(Offset + DataSize)
        End Get
    End Property

    ''' <summary>
    ''' Gets the size of the data.
    ''' </summary>
    ''' <value>The size of the data.</value>
    Private ReadOnly Property DataSize As UInt32
        Get
            'Not Width x Height due to padding
            Return CUInt(Math.Ceiling(Width * ColorDepth / 32) * 4 * Height)
        End Get
    End Property

    ''' <summary>
    ''' Gets the offset.
    ''' </summary>
    ''' <value>The offset.</value>
    Private ReadOnly Property Offset As UInt32
        Get
            'Header Size + Palette Size
            If IsIndexed Then
                Return CUInt(54 + _palette.Count)
            Else
                Return 54
            End If
        End Get
    End Property

    Private _palette As Byte()

    ''' <summary>
    ''' Gets or sets a value indicating whether this instance is indexed.
    ''' </summary>
    ''' <value>
    ''' <c>true</c> if this instance is indexed; otherwise, <c>false</c>.
    ''' </value>
    Public Property IsIndexed As Boolean

    ''' <summary>
    ''' Sets the palette.
    ''' </summary>
    ''' <param name="palette">The palette.</param>
    Public Sub SetPalette(ByVal palette As ColorPalette)
        ReDim _palette(palette.Length * 4 - 1)

        For i As Integer = 0 To palette.Length - 1
            _palette(4 * i + 0) = palette(i).B
            _palette(4 * i + 1) = palette(i).G
            _palette(4 * i + 2) = palette(i).R
        Next
    End Sub

    ''' <summary>
    ''' Sets the palette.
    ''' </summary>
    ''' <param name="palette">The palette.</param>
    Public Sub SetPalette(ByVal palette() As Byte)
        _palette = palette
    End Sub

    ''' <summary>
    ''' Applies a grey-scale palette.
    ''' </summary>
    Public Sub SetPalette()
        ReDim _palette(256 * 4 - 1)

        For i As Integer = 0 To 255
            _palette(4 * i + 0) = CByte(i)
            _palette(4 * i + 1) = CByte(i)
            _palette(4 * i + 2) = CByte(i)
        Next
    End Sub

    ''' <summary>
    ''' Gets or sets the color depth.
    ''' </summary>
    ''' <value>The color depth.</value>
    Public Property ColorDepth As UShort

    ''' <summary>
    ''' Gets or sets the width.
    ''' </summary>
    ''' <value>The width.</value>
    Public Property Width As Integer

    ''' <summary>
    ''' Gets or sets the height.
    ''' </summary>
    ''' <value>The height.</value>
    Public Property Height As Integer

    Private _writer As BinaryWriter
    Private _padding() As Byte

    ''' <summary>
    ''' Initializes a new instance of the <see cref="FastBitmap" /> class.
    ''' </summary>
    ''' <param name="destination">The destination.</param>
    Public Sub New(ByVal destination As Stream)
        'Initialize writer
        _writer = New BinaryWriter(destination, Encoding.Unicode)
    End Sub

    Private Sub WriteHeader()
        'padding
        ReDim _padding(CInt(Width * ColorDepth / 8 Mod 4 - 1))

        '### bitmap header ###

        'Magic Number "BM"
        _writer.Write(New Byte() {66, 77})

        'Size of entire file    
        _writer.Write(FileSize)

        'unused fields
        _writer.Write(New Byte() {0, 0, 0, 0})

        'position where to find the pixel information
        _writer.Write(Offset)

        '### DIP header (BITMAPINFOHEADER) ###

        'size of header
        _writer.Write(CUInt(40))

        'width
        _writer.Write(Width)

        'height, - to reverse row order
        _writer.Write(-Height)

        'number of color planes
        _writer.Write(CUShort(1))

        'color depth in bpp
        _writer.Write(ColorDepth)

        'compression method (none)
        _writer.Write(CUInt(0))

        'size of pixel data
        _writer.Write(DataSize)

        'horizontal resolution in pixel per meter
        _writer.Write(2835)

        'vertical resolution in pixel per meter
        _writer.Write(2835)

        'number of colors in the palette (0 to default to 2^bpp)
        _writer.Write(CUInt(0))

        'number of important colors
        _writer.Write(CUInt(0))

        '### palette ###
        'each pixel is described as 32bpp BGR0
        If IsIndexed then _writer.Write(_palette)
    End Sub

    Private _headerWritten As Boolean

    ''' <summary>
    ''' Writes the line. Once this method has been called no further changes on any property of this bitmap may be made.
    ''' </summary>
    ''' <param name="buffer">The buffer.</param>
    Public Sub WriteLine(ByVal buffer() As Byte)
        If Not _headerWritten Then
            WriteHeader()
            _headerWritten = True
        End If
    
        _writer.Write(buffer)

        If Width * ColorDepth / 8 Mod 4 <> 0 Then
            _writer.Write(_padding)
        End If
    End Sub

    ''' <summary>
    ''' Closes this instance.
    ''' </summary>
    Public Sub Close()
        _writer.Close()
    End Sub
End Class
