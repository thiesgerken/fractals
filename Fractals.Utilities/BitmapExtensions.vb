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
Imports System.Runtime.CompilerServices
Imports System.Windows
Imports System.Windows.Media.Imaging

Public Module BitmapExtensions

    ''' <summary>
    ''' Converts a Bitmap to a BitmapSource to be used in WPF applications.
    ''' </summary>
    ''' <param name="image">The image to convert.</param>
    ''' <returns></returns>
    <Extension()> _
    Public Function ToBitmapSource(ByVal image As Bitmap) As BitmapSource
        Return Interop.Imaging.CreateBitmapSourceFromHBitmap(image.GetHbitmap(), IntPtr.Zero, Int32Rect.Empty, System.Windows.Media.Imaging.BitmapSizeOptions.FromEmptyOptions())
    End Function

    ''' <summary>
    ''' Resizes the specified image.
    ''' </summary>
    ''' <param name="image">The image.</param>
    ''' <param name="width">The width.</param>
    ''' <param name="height">The height.</param>
    ''' <returns></returns>
    <Extension()> _
    Public Function Resize(ByVal image As Bitmap, ByVal width As Integer, ByVal height As Integer) As Bitmap
        If image Is Nothing Then Return Nothing

        Dim newImage As New Bitmap(width, height)
        newImage.SetResolution(96, 96)

        Dim g = Graphics.FromImage(newImage)

        g.CompositingQuality = Drawing2D.CompositingQuality.HighQuality
        g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBilinear
        g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

        g.DrawImage(image, New Rectangle(0, 0, width, height))
        g.Dispose()

        Return newImage
    End Function
End Module
