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
Imports System.Text

Public Class AsciiArtConverter

    ''' <summary>
    ''' Converts the image to ASCII text.
    ''' </summary>
    ''' <param name="img">The image to convert.</param>
    ''' <param name="lineBreak">string to use as line break.</param>
    ''' <param name="chars">The characters that are used, sorted by brightness.</param>
    ''' <returns></returns>
    Public Shared Function ConvertImageToAsciiText(ByVal img As Bitmap, ByVal lineBreak As String, ByVal chars As String()) As String
        Dim sb As New StringBuilder

        'characters are almost double as high as wide, so only each second line is used
        For y As Integer = 0 To img.Height - 1 Step 2
            For x As Integer = 0 To img.Width - 1
                Dim currentColor = img.GetPixel(x, y)
                Dim upperColor = currentColor
                Dim lowerColor = currentColor

                If y > 0 Then
                    upperColor = img.GetPixel(x, y - 1)
                End If

                If y < img.Height - 1 Then
                    lowerColor = img.GetPixel(x, y + 1)
                End If

                'convert to grayscale
                Dim currentAverage = (CInt(currentColor.R) + currentColor.G + currentColor.B) / 3
                Dim upperAverage = (CInt(upperColor.R) + upperColor.G + upperColor.B) / 3
                Dim lowerAverage = (CInt(lowerColor.R) + lowerColor.G + lowerColor.B) / 3

                'to prevent loss of information take the pixel above and below the current pixel into account
                Dim overallAverage = CInt((currentAverage * 0.5 + upperAverage * 0.25 + lowerAverage * 0.25) / 255 * chars.Length)

                sb.Append(chars(overallAverage))
            Next

            'last line does not need a linebreak
            If Not y = img.Height - 1 Then sb.Append(lineBreak)
        Next
        Return sb.ToString
    End Function

    ''' <summary>
    ''' Converts the image to ASCII text.
    ''' </summary>
    ''' <param name="img">The image to convert.</param>
    ''' <returns></returns>
    Public Shared Function ConvertImageToAsciiText(ByVal img As Bitmap) As String
        Return ConvertImageToAsciiText(img, Environment.NewLine, {"@", "#", "8", "&", "o", "*", ":", ".", " "})
    End Function

    ''' <summary>
    ''' Converts the image to ASCII HTML.
    ''' </summary>
    ''' <param name="img">The image to convert.</param>
    ''' <returns></returns>
    Public Shared Function ConvertImageToAsciiHtml(ByVal img As Bitmap) As String
        Dim sb As New StringBuilder

        sb.AppendLine("<!doctype html>")
        sb.AppendLine("<html>")
        sb.AppendLine("<body")
        sb.AppendLine("<p style=""font-family: Courier New;"">")

        sb.Append(ConvertImageToAsciiText(img, "<br />" & Environment.NewLine, {"@", "#", "8", "&amp;", "o", "*", ":", ".", " "}))

        sb.AppendLine("</p>")
        sb.AppendLine("</body>")
        sb.AppendLine("</html>")

        Return sb.ToString
    End Function
End Class
