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
Imports System.Drawing
Imports System.Runtime.InteropServices
Imports Microsoft.WindowsAPICodePack.ShellExtensions
Imports System.Threading

<ClassInterface(ClassInterfaceType.None), Guid("369CA96C-DE04-4e3e-8016-62CFEE456BAE"), ComVisible(True), ThumbnailProvider("FractalThumbnailProvider", ".fractal")>
Public Class FractalThumbnailProvider
    Inherits ThumbnailProvider
    Implements IThumbnailFromStream

#Region "IThumbnailFromStream Members"

    ''' <summary>
    ''' Constructs the bitmap.
    ''' </summary>
    ''' <param name="stream">The stream.</param>
    ''' <param name="sideSize">Size of the side.</param>
    ''' <returns></returns>
    Public Function ConstructBitmap(ByVal stream As Stream, ByVal sideSize As Integer) As Bitmap Implements IThumbnailFromStream.ConstructBitmap
        Try
            Dim f = Fractal.Load(stream)

            'Buddhabrots do not produce good results when scaled down
            'and are very calculation intensive
            If f.GetType Is GetType(Buddhabrot) Then Return Nothing

            f.Size = New Size(sideSize, sideSize)

            'minimize cpu usage
            f.AntiAliasingSamples = 1

            f.StartRendering()

            Dim t As New Thread(Sub() f.Join())
            t.Start()

            Dim stp As New Stopwatch
            stp.Start()

            'wait max 10s
            While t.IsAlive And stp.ElapsedMilliseconds < 10000
                Thread.Sleep(10)
            End While

            stp.Stop()

            'finished?
            If t.IsAlive Then
                f.AbortRendering()

                Return Nothing
            Else
                Return f.Image
            End If
        Catch ex As Exception
            Return Nothing
        End Try
    End Function

#End Region

End Class
