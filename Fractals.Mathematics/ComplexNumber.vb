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
Imports System.IO
Imports System.Xml
Imports System.Globalization

''' <summary>
''' Provides a class to calculate with complex numbers. The .Net Framework provides a class for this, but I had to discover that my implementation is much faster.
''' </summary>
Public Structure ComplexNumber
    Implements IEquatable(Of ComplexNumber)

#Region "Fields"

    Private _realPart As Double
    Private _imaginaryPart As Double

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets an undefined complex number.
    ''' </summary>
    ''' <value></value>
    Public Shared ReadOnly Property NaN As ComplexNumber
        Get
            Return New ComplexNumber(Double.NaN, Double.NaN)
        End Get
    End Property

    ''' <summary>
    ''' Gets or sets the real part.
    ''' </summary>
    ''' <value>The real part.</value>
    Public Property RealPart As Double
        Get
            Return _realPart
        End Get
        Set(ByVal value As Double)
            _realPart = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the imaginary part.
    ''' </summary>
    ''' <value>The imaginary part.</value>
    Public Property ImaginaryPart As Double
        Get
            Return _imaginaryPart
        End Get
        Set(ByVal value As Double)
            _imaginaryPart = value
        End Set
    End Property

    ''' <summary>
    ''' Gets the angle between this instance and the real axis.
    ''' </summary>
    ''' <value>The angle</value>
    Public ReadOnly Property Phi As Double
        Get
            Dim absVal = ComplexNumber.Abs(Me)

            If absVal = 0 Then Return 0

            Dim result As Double

            If ImaginaryPart >= 0 Then
                result = Math.Acos(RealPart / absVal)
            Else
                result = -Math.Acos(RealPart / absVal)
            End If

            If result < 0 Then
                result += Math.PI * 2
            End If

            Return result
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="ComplexNumber" /> class.
    ''' </summary>
    ''' <param name="realPart">The real part.</param>
    ''' <param name="imaginaryPart">The imaginary part.</param>
    Public Sub New(ByVal realPart As Double, ByVal imaginaryPart As Double)
        Me.RealPart = realPart
        Me.ImaginaryPart = imaginaryPart
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Determines whether a specified instance is undefined.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <returns>
    ''' <c>true</c> if is NaN; otherwise, <c>false</c>.
    ''' </returns>
    Public Shared Function IsNaN(ByVal obj As ComplexNumber) As Boolean
        Return Double.IsNaN(obj.RealPart) And Double.IsNaN(obj.ImaginaryPart)
    End Function

    ''' <summary>
    ''' Returns the complex argument of a given complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Arg(ByVal z As ComplexNumber) As Double
        Return z.Phi
    End Function

    ''' <summary>
    ''' Returns the absolute value of a specified complex number
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Abs(ByVal z As ComplexNumber) As Double
        Return Math.Sqrt(z._realPart * z._realPart + z.ImaginaryPart * z._imaginaryPart)
    End Function

    ''' <summary>
    ''' Returns the squared absolute value of a specified complex number, which is faster than computing the 'real' absolute value since there is no square root necessary.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function AbsSquared(ByVal z As ComplexNumber) As Double
        Return z._realPart * z._realPart + z._imaginaryPart * z._imaginaryPart
    End Function

    ''' <summary>
    ''' Gets the complex conjugate of a given complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Conjugate(ByVal z As ComplexNumber) As ComplexNumber
        Return New ComplexNumber(z._realPart, -1 * z._imaginaryPart)
    End Function

    ''' <summary>
    ''' Computes the Sine of a given complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Sin(ByVal z As ComplexNumber) As ComplexNumber
        'z  = a + bi
        'a* = sin(a) cosh(b)
        'b* = cos(a) sinh(b) 
        Return New ComplexNumber(Math.Sin(z._realPart) * Math.Cosh(z._imaginaryPart), Math.Cos(z._realPart) * Math.Sinh(z._imaginaryPart))
    End Function

    ''' <summary>
    ''' Computes the Cosine of a given complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Cos(ByVal z As ComplexNumber) As ComplexNumber
        Return (Exp(New ComplexNumber(0, 1) * z) + (New ComplexNumber(0, -1) * z)) / 2
    End Function

    ''' <summary>
    ''' Rounds a specified complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <param name="digits">The maximum count digits in the result.</param>
    ''' <returns></returns>
    Public Shared Function Round(ByVal z As ComplexNumber, ByVal digits As Integer) As ComplexNumber
        Return New ComplexNumber(Math.Round(z._realPart, digits), Math.Round(z._imaginaryPart, digits))
    End Function

    ''' <summary>
    ''' Returns the natural (base e) logarithm of a complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Log(ByVal z As ComplexNumber) As ComplexNumber
        'ln(z) = ln(|z|) + phi*i 
        Return New ComplexNumber(Math.Log(ComplexNumber.Abs(z)), z.Phi)
    End Function

    ''' <summary>
    ''' Returns e raised to a given complex number.
    ''' </summary>
    ''' <param name="z">The complex number.</param>
    ''' <returns></returns>
    Public Shared Function Exp(ByVal z As ComplexNumber) As ComplexNumber
        'e^(x+yi) = e^x * e^(yi) = e^x * (cos(y) + sin(y) * i)

        Dim expx = Math.Exp(z._realPart)
        Return New ComplexNumber(expx * Math.Cos(z._imaginaryPart), expx * Math.Sin(z._imaginaryPart))
    End Function

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overrides Function ToString() As String
        Return Me.ToString(0)
    End Function

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <param name="decimals">The maximum count of decimals places in the string.</param>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overloads Function ToString(ByVal decimals As Integer) As String
        Dim s As String = ""

        If decimals < 0 Then decimals = 0
        If decimals > 15 Then decimals = 15

        If Me.RealPart <> 0 Then
            If decimals = 0 Then
                s &= Me.RealPart.ToString
            Else
                s &= Math.Round(Me.RealPart, decimals)
            End If
        End If

        If Me.RealPart <> 0 And Me.ImaginaryPart > 0 Then
            s &= "+"
        End If

        If Me.ImaginaryPart <> 0 Then
            If Me.ImaginaryPart = -1 Then
                s &= "-"
            ElseIf Me.ImaginaryPart = 1 Then
            ElseIf decimals = 0 Then
                s &= Me.ImaginaryPart.ToString
            Else
                s &= Math.Round(Me.ImaginaryPart, decimals)
            End If

            s &= "i"
        End If

        If s = "" Then s = "0"

        Return s
    End Function

    ''' <summary>
    ''' Parses the specified value.
    ''' </summary>
    ''' <param name="value">The value.</param>
    ''' <returns></returns>
    Public Shared Function Parse(ByVal value As String, ByVal format As CultureInfo) As ComplexNumber
        Dim firstSign As String = "+"
        Dim secondSign As String = "+"

        If value.StartsWith("-") Then
            value = value.Substring(1)
            firstSign = "-"
        End If

        If value.Contains("-") Then
            value = value.Replace("-"c, "+"c)
            secondSign = "-"
        End If

        Dim re As String
        Dim im As String

        If value.Contains("+"c) Then
            Dim values() = value.Split("+"c)

            values(0) = firstSign & values(0)
            values(1) = secondSign & values(1)

            If values(0).Contains("i") Then
                im = values(0).Replace("i", "")
                re = values(1)
            Else
                re = values(0)
                im = values(1).Replace("i", "")
            End If
        Else
            value = firstSign & value

            If value.Contains("i") Then
                re = "0"
                im = value.Replace("i", "")
            Else
                re = value
                im = "+0"
            End If
        End If

        If im.Length = 1 Then im &= "1"

        Return New ComplexNumber(Double.Parse(re, format), Double.Parse(im, format))
    End Function
    ''' <summary>
    ''' Equalses the specified other.
    ''' </summary>
    ''' <param name="other">The other.</param>
    ''' <returns></returns>
    Public Overloads Function Equals(ByVal other As ComplexNumber) As Boolean Implements System.IEquatable(Of ComplexNumber).Equals
        Return _realPart = other.RealPart And _imaginaryPart = other.ImaginaryPart
    End Function

    ''' <summary>
    ''' Saves the specified obj.
    ''' </summary>
    ''' <param name="obj">The obj.</param>
    ''' <param name="x">The x.</param>
    Public Shared Sub Save(ByVal obj As ComplexNumber, ByVal x As XmlWriter)
        x.WriteStartElement("RealPart")
        x.WriteValue(obj.RealPart)
        x.WriteEndElement()

        x.WriteStartElement("ImaginaryPart")
        x.WriteValue(obj.ImaginaryPart)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the specified x.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns></returns>
    Public Shared Function Load(ByVal x As XElement) As ComplexNumber
        Dim z As New ComplexNumber

        z.ImaginaryPart = Double.Parse(x.Element("ImaginaryPart").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)
        z.RealPart = Double.Parse(x.Element("RealPart").Value, Globalization.CultureInfo.InvariantCulture.NumberFormat)

        Return z
    End Function

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator +.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator +(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As ComplexNumber
        Return New ComplexNumber(x.RealPart + y.RealPart, x.ImaginaryPart + y.ImaginaryPart)
    End Operator


    ''' <summary>
    ''' Implements the operator -.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator -(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As ComplexNumber
        Return New ComplexNumber(x.RealPart - y.RealPart, x.ImaginaryPart - y.ImaginaryPart)
    End Operator

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator *(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As ComplexNumber
        Return New ComplexNumber(x.RealPart * y.RealPart - x.ImaginaryPart * y.ImaginaryPart, x.RealPart * y.ImaginaryPart + x.ImaginaryPart * y.RealPart)
    End Operator

    ''' <summary>
    ''' Implements the operator /.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator /(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As ComplexNumber
        Return New ComplexNumber((x.RealPart * y.RealPart + x.ImaginaryPart * y.ImaginaryPart) / (y.RealPart * y.RealPart + y.ImaginaryPart * y.ImaginaryPart), (x.ImaginaryPart * y.RealPart - x.RealPart * y.ImaginaryPart) / (y.RealPart * y.RealPart + y.ImaginaryPart * y.ImaginaryPart))
    End Operator

    ''' <summary>
    ''' Implements the operator =.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator =(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As Boolean
        Return x.RealPart = y.RealPart And x.ImaginaryPart = y.ImaginaryPart
    End Operator

    ''' <summary>
    ''' Implements the operator &lt;&gt;.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator <>(ByVal x As ComplexNumber, ByVal y As ComplexNumber) As Boolean
        Return Not x = y
    End Operator

    ''' <summary>
    ''' Implements the operator ^.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator ^(ByVal x As ComplexNumber, ByVal y As Integer) As ComplexNumber
        Dim value = x
        Dim exponent = Math.Abs(y)

        If x.RealPart = 0 And x.ImaginaryPart = 0 Then Return 0

        If exponent > 1 Then
            For i As Integer = 0 To exponent - 2
                value = value * x
            Next
        End If

        If y < 0 Then value = 1 / value

        Return value
    End Operator

    ''' <summary>
    ''' Implements the operator ^.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <param name="y">The y.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator ^(ByVal x As ComplexNumber, ByVal y As Double) As ComplexNumber
        'try to use faster formula for integer powers
        If y = Math.Truncate(y) Then Return x ^ CInt(y)

        '0^y = 0 
        If AbsSquared(x) = 0 Then Return 0

        'z^w = e^(w*ln(z))
        Return Exp(y * Log(x))
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Narrowing Operator CType(ByVal x As ComplexNumber) As Double
        Return x.RealPart
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Widening Operator CType(ByVal x As Double) As ComplexNumber
        Return New ComplexNumber(x, 0)
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Widening Operator CType(ByVal p As PointF) As ComplexNumber
        Return New ComplexNumber(p.X, p.Y)
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="p">The p.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Widening Operator CType(ByVal p As Point) As ComplexNumber
        Return New ComplexNumber(p.X, p.Y)
    End Operator

#End Region
End Structure
