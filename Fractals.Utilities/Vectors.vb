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

Imports System.Runtime.InteropServices
Imports System.Drawing

''' <summary>
''' Vector types for OpenCL - .Net interop
''' </summary>
Public Class Vectors

    ''' <summary>
    ''' Structure of 4 UInts in Order RGBA
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure UInt4
        Public R As UInteger
        Public G As UInteger
        Public B As UInteger
        Public A As UInteger

        ''' <summary>
        ''' Initializes a new instance of the <see cref="UInt4" /> struct.
        ''' </summary>
        ''' <param name="r">The r.</param>
        ''' <param name="g">The g.</param>
        ''' <param name="b">The b.</param>
        ''' <param name="a">A.</param>
        Public Sub New(ByVal r As UInteger, ByVal g As UInteger, ByVal b As UInteger, ByVal a As UInteger)
            Me.R = r
            Me.G = g
            Me.B = b
            Me.A = a
        End Sub
    End Structure


    ''' <summary>
    ''' Structure of 4 Bytes in Order RGBA
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Byte4
        Public R As Byte
        Public G As Byte
        Public B As Byte
        Public A As Byte

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Byte4" /> struct.
        ''' </summary>
        ''' <param name="r">The r.</param>
        ''' <param name="g">The g.</param>
        ''' <param name="b">The b.</param>
        ''' <param name="a">A.</param>
        Public Sub New(ByVal r As Byte, ByVal g As Byte, ByVal b As Byte, ByVal a As Byte)
            Me.R = r
            Me.G = g
            Me.B = b
            Me.A = a
        End Sub

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Byte4" /> struct.
        ''' </summary>
        ''' <param name="col">The col.</param>
        Public Sub New(ByVal col As Color)
            Me.R = col.R
            Me.G = col.G
            Me.B = col.B
            Me.A = col.A
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 4 Ints in Order XYZW
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Int4
        Public X As Integer
        Public Y As Integer
        Public Z As Integer
        Public W As Integer

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Int4" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        ''' <param name="z">The z.</param>
        ''' <param name="w">The w.</param>
        Public Sub New(ByVal x As Integer, ByVal y As Integer, ByVal z As Integer, ByVal w As Integer)
            Me.x = x
            Me.y = y
            Me.z = z
            Me.w = w
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 2 UInts in Order XY
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure UInt2
        Public X As UInteger
        Public Y As UInteger

        ''' <summary>
        ''' Initializes a new instance of the <see cref="UInt2" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        Public Sub New(ByVal x As UInteger, ByVal y As UInteger)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 2 Ints in Order XY
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Int2
        Public X As Integer
        Public Y As Integer

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Int2" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        Public Sub New(ByVal x As Integer, ByVal y As Integer)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 2 Doubles in Order XY
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Double2
        Public X As Double
        Public Y As Double

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Double2" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        Public Sub New(ByVal x As Double, ByVal y As Double)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 8 Floats in Order S0 ... S7
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Float8
        Public S0 As Single
        Public S1 As Single
        Public S2 As Single
        Public S3 As Single
        Public S4 As Single
        Public S5 As Single
        Public S6 As Single
        Public S7 As Single

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Float8" /> struct.
        ''' </summary>
        ''' <param name="s0">The s0.</param>
        ''' <param name="s1">The s1.</param>
        ''' <param name="s2">The s2.</param>
        ''' <param name="s3">The s3.</param>
        ''' <param name="s4">The s4.</param>
        ''' <param name="s5">The s5.</param>
        ''' <param name="s6">The s6.</param>
        ''' <param name="s7">The s7.</param>
        Public Sub New(ByVal s0 As Single, ByVal s1 As Single, ByVal s2 As Single, ByVal s3 As Single, ByVal s4 As Single, ByVal s5 As Single, ByVal s6 As Single, ByVal s7 As Single)
            Me.S0 = s0
            Me.S1 = s1
            Me.S2 = s2
            Me.S3 = s3
            Me.S4 = s4
            Me.S5 = s5
            Me.S6 = s6
            Me.S7 = s7
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 2 Floats in Order XY
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Float2
        Public X As Single
        Public Y As Single

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Float2" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        Public Sub New(ByVal x As Single, ByVal y As Single)
            Me.X = x
            Me.Y = y
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 8 Doubles in Order S0 ... S7
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Double8
        Public S0 As Double
        Public S1 As Double
        Public S2 As Double
        Public S3 As Double
        Public S4 As Double
        Public S5 As Double
        Public S6 As Double
        Public S7 As Double

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Double8" /> struct.
        ''' </summary>
        ''' <param name="s0">The s0.</param>
        ''' <param name="s1">The s1.</param>
        ''' <param name="s2">The s2.</param>
        ''' <param name="s3">The s3.</param>
        ''' <param name="s4">The s4.</param>
        ''' <param name="s5">The s5.</param>
        ''' <param name="s6">The s6.</param>
        ''' <param name="s7">The s7.</param>
        Public Sub New(ByVal s0 As Double, ByVal s1 As Double, ByVal s2 As Double, ByVal s3 As Double, ByVal s4 As Double, ByVal s5 As Double, ByVal s6 As Double, ByVal s7 As Double)
            Me.S0 = s0
            Me.S1 = s1
            Me.S2 = s2
            Me.S3 = s3
            Me.S4 = s4
            Me.S5 = s5
            Me.S6 = s6
            Me.S7 = s7
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 5 Doubles in Order S0 ... S4
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Double5
        Public S0 As Double
        Public S1 As Double
        Public S2 As Double
        Public S3 As Double
        Public S4 As Double
       
        ''' <summary>
        ''' Initializes a new instance of the <see cref="Double8" /> struct.
        ''' </summary>
        ''' <param name="s0">The s0.</param>
        ''' <param name="s1">The s1.</param>
        ''' <param name="s2">The s2.</param>
        ''' <param name="s3">The s3.</param>
        ''' <param name="s4">The s4.</param>
        Public Sub New(ByVal s0 As Double, ByVal s1 As Double, ByVal s2 As Double, ByVal s3 As Double, ByVal s4 As Double)
            Me.S0 = s0
            Me.S1 = s1
            Me.S2 = s2
            Me.S3 = s3
            Me.S4 = s4
        End Sub
    End Structure

    ''' <summary>
    ''' Structure of 4 Doubles in Order x y z w
    ''' </summary>
    <StructLayout(LayoutKind.Sequential)>
    Public Structure Double4
        Public X As Double
        Public Y As Double
        Public Z As Double
        Public W As Double

        ''' <summary>
        ''' Initializes a new instance of the <see cref="Double4" /> struct.
        ''' </summary>
        ''' <param name="x">The x.</param>
        ''' <param name="y">The y.</param>
        ''' <param name="z">The z.</param>
        ''' <param name="w">The w.</param>
        Public Sub New(ByVal x As Double, ByVal y As Double, ByVal z As Double, ByVal w As Double)
            Me.X = x
            Me.Y = y
            Me.Z = z
            Me.W = w
        End Sub
    End Structure
End Class
